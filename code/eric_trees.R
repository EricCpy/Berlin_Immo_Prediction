train_data1 <- train_data[[1]]
test_data1 <- test_data[[1]]

full_tree <- rpart(
  baseRent ~ .,
  data = train_data1,
  method = "anova",
  cp = 0
)

default_tree <- rpart(
  baseRent ~ .,
  data = train_data1,
  method = "anova"
)

printcp(default_tree)

print(head(full_tree$cptable, 10))
plotcp(full_tree)



best_cp <- full_tree$cptable[which.min(full_tree$cptable[, "xerror"]), "CP"]
best_cp

cptable <- default_tree$cptable
minDeviance <- which.min(cptable[, "xerror"])
dotted <- cptable[minDeviance, 4] + cptable[minDeviance, 5]
cpPruning <- cptable[cptable[, "xerror"] < dotted, ][1, ]
cpPruning[1]

pruned_tree <- prune(full_tree, cp = best_cp)
pruned_tree$terms
rpart.plot(default_tree, ) 

train_pred_full <- predict(full_tree, train_data1)
test_pred_full <- predict(full_tree, test_data1)
train_pred_default <- predict(default_tree, train_data1)
test_pred_default <- predict(default_tree, test_data1)
train_pred_pruned <- predict(pruned_tree, train_data1)
test_pred_pruned <- predict(pruned_tree, test_data1)

train_metrics_full <- metrics(train_data1$baseRent, train_pred_full)
test_metrics_full <- metrics(test_data1$baseRent, test_pred_full)
train_metrics_default <- metrics(train_data1$baseRent, train_pred_default)
test_metrics_default <- metrics(test_data1$baseRent, test_pred_default)
train_metrics_pruned <- metrics(train_data1$baseRent, train_pred_pruned)
test_metrics_pruned <- metrics(test_data1$baseRent, test_pred_pruned)

cat("Full Tree Metrics (Training): MSE =", train_metrics_full["MSE"], "MAE =", train_metrics_full["MAE"], "\n")
cat("Full Tree Metrics (Testing): MSE =", test_metrics_full["MSE"], "MAE =", test_metrics_full["MAE"], "\n")
cat("Default Tree Metrics (Training): MSE =", train_metrics_default["MSE"], "MAE =", train_metrics_default["MAE"], "\n")
cat("Default Tree Metrics (Testing): MSE =", test_metrics_default["MSE"], "MAE =", test_metrics_default["MAE"], "\n")
cat("Pruned Tree Metrics (Training): MSE =", train_metrics_pruned["MSE"], "MAE =", train_metrics_pruned["MAE"], "\n")
cat("Pruned Tree Metrics (Testing): MSE =", test_metrics_pruned["MSE"], "MAE =", test_metrics_pruned["MAE"], "\n")