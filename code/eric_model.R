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
predict_function_rf <- function(model, test_data) {
  test_data_rf <- test_data %>%
    select(where(~ !(is.factor(.) || is.character(.)) || n_distinct(.) <= 53))
  
  predict(model, test_data_rf)
}

train_model_function_rf <- function(train_data) {
  train_data_rf <- train_data %>%
    select(where(~ !(is.factor(.) || is.character(.)) || n_distinct(.) <= 53))
  
  randomForest(
    baseRent ~ ., 
    data = train_data_rf,
    ntree=500,
    mtry=20,
    nodesize=2
  )
}

results <- train_and_evaluate_models(
  train_data = train_data,
  test_data = test_data,
  train_model_function = train_model_function_rf,
  predict_function = predict_function_rf,
  saved_models = "data/models/models_randomforest.rds",
  save_model = TRUE
)

print(results)

# ---- GBM -----
predict_function_gbm <- function(model, test_data) {
  test_data_bool_as_factor <- test_data %>%
    mutate(across(where(is.logical), as.factor))
  
  predict(model, test_data_bool_as_factor)
}

train_model_function_gbm <- function(train_data) {
  train_data_bool_as_factor <- train_data %>%
    mutate(across(where(is.logical), as.factor))
  
  gbm(
    baseRent ~ .,
    data = train_data_bool_as_factor,
    distribution = "gaussian",
    n.trees = 500,            
    interaction.depth = 6,     
    shrinkage = 0.05,        
    n.minobsinnode = 10       
  )
}

results <- train_and_evaluate_models(
  train_data = train_data,
  test_data = test_data,
  train_model_function = train_model_function_gbm,
  predict_function = predict_function_gbm,
  saved_models = "data/models/models_gbm.rds",
  save_model = TRUE
)

print(results)


# ---- XGBOOST -----
predict_function_xgb <- function(model, test_data) {
  # One hot encoding
  dummies <- dummyVars(baseRent ~ ., data = test_data)
  test_encoded <- data.frame(predict(dummies, newdata = test_data))
  test_matrix <- xgb.DMatrix(data = as.matrix(test_encoded), label = test_data$baseRent)
  
  predict(model, test_matrix)
}

train_model_function_xgb <- function(train_data) {
  # One hot encoding
  dummies <- dummyVars(baseRent ~ ., data = train_data)
  train_encoded <- data.frame(predict(dummies, newdata = train_data))
  train_matrix <- xgb.DMatrix(data = as.matrix(train_encoded), label = train_data$baseRent)
  
  xgb_model_best <- xgboost(
    data = train_matrix,
    objective = "reg:squarederror",
    nrounds = 1000,
    eta = 0.1,
    max_depth=6,
    min_child_weight=2,
    subsample=0.8,
    colsample_bytree=0.6,
    verbose = FALSE
  )
}

results <- train_and_evaluate_models(
  train_data = train_data,
  test_data = test_data,
  train_model_function = train_model_function_xgb,
  predict_function = predict_function_xgb,
  saved_models = "",
  save_model = FALSE
)

print(results)
