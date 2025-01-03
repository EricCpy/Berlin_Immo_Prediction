source("code/setup.R")

# View the cleaned dataframe
head(berlin_data_for_model)
dim(berlin_data_for_model)
colnames(berlin_data_for_model)
sum(is.na(berlin_data_for_model$baseRent))

set.seed(123)
train_index <- sample(seq_len(nrow(berlin_data_for_model)), size = 0.8 * nrow(berlin_data_for_model))
train_data <- berlin_data_for_model[train_index, ]
test_data <- berlin_data_for_model[-train_index, ]

# ---- Decision Trees ----

full_tree <- rpart(
  baseRent ~ .,
  data = train_data,
  method = "anova",
  cp = 0
)

default_tree <- rpart(
  baseRent ~ .,
  data = train_data,
  method = "anova",
  cp = 0
)

printcp(full_tree)
plotcp(full_tree)

best_cp <- full_tree$cptable[which.min(full_tree$cptable[, "xerror"]), "CP"]
best_cp
pruned_tree <- prune(full_tree, cp = best_cp)
pruned_tree
rpart.plot(pruned_tree)

train_pred_full <- predict(full_tree, train_data)
test_pred_full <- predict(full_tree, test_data)
train_pred_default <- predict(default_tree, train_data)
test_pred_default <- predict(default_tree, test_data)
train_pred_pruned <- predict(pruned_tree, train_data)
test_pred_pruned <- predict(pruned_tree, test_data)

train_metrics_full <- metrics(train_data$baseRent, train_pred_full)
test_metrics_full <- metrics(test_data$baseRent, test_pred_full)
train_metrics_default <- metrics(train_data$baseRent, train_pred_full)
test_metrics_default <- metrics(test_data$baseRent, test_pred_full)
train_metrics_pruned <- metrics(train_data$baseRent, train_pred_pruned)
test_metrics_pruned <- metrics(test_data$baseRent, test_pred_pruned)

cat("Full Tree Metrics (Training): MSE =", train_metrics_full["MSE"], "MAE =", train_metrics_full["MAE"], "\n")
cat("Full Tree Metrics (Testing): MSE =", test_metrics_full["MSE"], "MAE =", test_metrics_full["MAE"], "\n")
cat("Pruned Tree Metrics (Training): MSE =", train_metrics_pruned["MSE"], "MAE =", train_metrics_pruned["MAE"], "\n")
cat("Pruned Tree Metrics (Testing): MSE =", test_metrics_pruned["MSE"], "MAE =", test_metrics_pruned["MAE"], "\n")

# ---- SVMs ----

default_svm <- svm(baseRent ~ ., data = train_data, kernel = "radial")

tuned_svm <- tune(
  svm,
  baseRent ~ .,
  data = train_data,
  ranges = list(cost = 2^(-1:2), gamma = 2^(-2:1))
)

best_svm <- tuned_svm$best.model

train_pred_default <- predict(default_svm, train_data)
test_pred_default <- predict(default_svm, test_data)
train_pred_tuned <- predict(best_svm, train_data)
test_pred_tuned <- predict(best_svm, test_data)

train_metrics_default <- metrics(train_data$baseRent, train_pred_default)
test_metrics_default <- metrics(test_data$baseRent, test_pred_default)

train_metrics_tuned <- metrics(train_data$baseRent, train_pred_tuned)
test_metrics_tuned <- metrics(test_data$baseRent, test_pred_tuned)

cat("Default SVM Metrics (Training): MSE =", train_metrics_default["MSE"], "MAE =", train_metrics_default["MAE"], "\n")
cat("Default SVM Metrics (Testing): MSE =", test_metrics_default["MSE"], "MAE =", test_metrics_default["MAE"], "\n")
cat("Tuned SVM Metrics (Training): MSE =", train_metrics_tuned["MSE"], "MAE =", train_metrics_tuned["MAE"], "\n")
cat("Tuned SVM Metrics (Testing): MSE =", test_metrics_tuned["MSE"], "MAE =", test_metrics_tuned["MAE"], "\n")

# ---- Ensemble Methods ----


# ---- Test Stuff ----

