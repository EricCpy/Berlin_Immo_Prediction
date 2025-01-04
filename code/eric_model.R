source("code/setup.R")

# ---- Decision Trees ----
train_model_function_tree_full <- function(train_data) {
  rpart(
    baseRent ~ .,
    data = train_data,
    method = "anova",
    cp = 0
  )
}

predict_function_tree <- function(model, test_data) {
  predict(model, test_data)
}

## ---- Decision Tree Full ----
results <- train_and_evaluate_models(
  train_data = train_data,
  test_data = test_data,
  train_model_function = train_model_function_tree_full,
  predict_function = predict_function_tree,
  saved_models = "data/models/models_tree_full.rds",
  save_model = TRUE
)

print(results)

## ---- Decision Trees Prune Best Xerror ----

train_model_function_tree_prune_xerror <- function(train_data) {
  tree <- train_model_function_tree_full(train_data)
  best_cp <- tree$cptable[which.min(tree$cptable[, "xerror"]), "CP"]
  pruned_tree <- prune(tree, cp = best_cp)
  pruned_tree
}

results <- train_and_evaluate_models(
  train_data = train_data,
  test_data = test_data,
  train_model_function = train_model_function_tree_prune_xerror,
  predict_function = predict_function_tree,
  saved_models = "data/models/models_tree_pruned_xerror.rds",
  save_model = TRUE
)

print(results)

## ---- Decision Trees Prune "Best" ----

train_model_function_tree_prune_best <- function(train_data) {
  tree <- train_model_function_tree_full(train_data)
  
  cptable <- tree$cptable
  minDeviance <- which.min(cptable[, "xerror"])
  dotted <- cptable[minDeviance, 4] + cptable[minDeviance, 5]
  best_cp <- cptable[cptable[, "xerror"] < dotted, ][1, ]
  pruned_tree <- prune(tree, cp = best_cp[1])
  pruned_tree
}

results <- train_and_evaluate_models(
  train_data = train_data,
  test_data = test_data,
  train_model_function = train_model_function_tree_prune_best,
  predict_function = predict_function_tree,
  saved_models = "data/models/models_tree_pruned_best.rds",
  save_model = TRUE
)

print(results)




# ---- SVMs ----
predict_function_svm <- function(model, test_data) {
  predict(model, test_data)
}

train_model_function_svm <- function(train_data) {
  svm(baseRent ~ ., data = train_data, coef=5, kernel = "polynomial")
}

results <- train_and_evaluate_models(
  train_data = train_data,
  test_data = test_data,
  train_model_function = train_model_function_svm,
  predict_function = predict_function_svm,
  saved_models = "data/models/models_svm.rds",
  save_model = TRUE
)

print(results)

# ---- Randomforests ----
default_rf <- randomForest(
  baseRent ~ ., 
  data = train_data1
)

# Tune Random Forest using cross-validation
tuned_rf <- tune(
  randomForest,
  baseRent ~ .,
  data = train_data1,
  ranges = list(mtry = c(2, 3, 4, 5, 6)) # Specify other values to try
)

best_rf <- tuned_rf$best.model

train_pred_default <- predict(default_rf, train_data1)
test_pred_default <- predict(default_rf, test_data1)
train_pred_tuned <- predict(best_rf, train_data1)
test_pred_tuned <- predict(best_rf, test_data1)

train_metrics_default <- metrics(train_data1$baseRent, train_pred_default)
test_metrics_default <- metrics(test_data1$baseRent, test_pred_default)
train_metrics_tuned <- metrics(train_data1$baseRent, train_pred_tuned)
test_metrics_tuned <- metrics(test_data1$baseRent, test_pred_tuned)

cat("Default Random Forest Metrics (Training): MSE =", train_metrics_default["MSE"], "MAE =", train_metrics_default["MAE"], "\n")
cat("Default Random Forest Metrics (Testing): MSE =", test_metrics_default["MSE"], "MAE =", test_metrics_default["MAE"], "\n")
cat("Tuned Random Forest Metrics (Training): MSE =", train_metrics_tuned["MSE"], "MAE =", train_metrics_tuned["MAE"], "\n")
cat("Tuned Random Forest Metrics (Testing): MSE =", test_metrics_tuned["MSE"], "MAE =", test_metrics_tuned["MAE"], "\n")

# Variable importance for tuned model
importance(best_rf)
varImpPlot(best_rf)

# ---- GBM -----
# Load necessary libraries


# Default GBM model (no tuning)
default_gbm <- gbm(
  baseRent ~ .,
  data = train_data1,
  distribution = "gaussian", # For regression
  n.trees = 100,             # Default number of trees
  interaction.depth = 1,     # Default depth
  shrinkage = 0.1,           # Default learning rate
  cv.folds = 0,              # No cross-validation for the default model
  n.minobsinnode = 10        # Default minimum number of observations in a terminal node
)

# Tune GBM hyperparameters using caret
train_control <- trainControl(
  method = "cv",    # Cross-validation
  number = 5,       # Number of folds
  verboseIter = TRUE # Show progress
)

tuned_gbm <- train(
  baseRent ~ ., 
  data = train_data1, 
  method = "gbm", 
  trControl = train_control,
  tuneGrid = expand.grid(
    n.trees = c(100, 200, 300),         # Number of trees
    interaction.depth = c(1, 3, 5),     # Tree depth
    shrinkage = c(0.1, 0.05),           # Learning rate
    n.minobsinnode = c(10, 20)          # Minimum observations in a node
  ),
  verbose = FALSE
)

# Best tuned GBM model
best_gbm <- tuned_gbm$finalModel
cat("Best hyperparameters:\n")
print(tuned_gbm$bestTune)

# Predictions for default and tuned models
train_pred_default <- predict(default_gbm, newdata = train_data1, n.trees = 100)
test_pred_default <- predict(default_gbm, newdata = test_data1, n.trees = 100)
train_pred_tuned <- predict(best_gbm, newdata = train_data1, n.trees = tuned_gbm$bestTune$n.trees)
test_pred_tuned <- predict(best_gbm, newdata = test_data1, n.trees = tuned_gbm$bestTune$n.trees)

# Define a custom metrics function
metrics <- function(actual, predicted) {
  mse <- mean((actual - predicted)^2)
  mae <- mean(abs(actual - predicted))
  return(c(MSE = mse, MAE = mae))
}

# Evaluate metrics
train_metrics_default <- metrics(train_data1$baseRent, train_pred_default)
test_metrics_default <- metrics(test_data1$baseRent, test_pred_default)

train_metrics_tuned <- metrics(train_data1$baseRent, train_pred_tuned)
test_metrics_tuned <- metrics(test_data1$baseRent, test_pred_tuned)

# Display metrics
cat("Default GBM Metrics (Training): MSE =", train_metrics_default["MSE"], 
    "MAE =", train_metrics_default["MAE"], "\n")
cat("Default GBM Metrics (Testing): MSE =", test_metrics_default["MSE"], 
    "MAE =", test_metrics_default["MAE"], "\n")
cat("Tuned GBM Metrics (Training): MSE =", train_metrics_tuned["MSE"], 
    "MAE =", train_metrics_tuned["MAE"], "\n")
cat("Tuned GBM Metrics (Testing): MSE =", test_metrics_tuned["MSE"], 
    "MAE =", test_metrics_tuned["MAE"], "\n")



# ---- Test Stuff ----

