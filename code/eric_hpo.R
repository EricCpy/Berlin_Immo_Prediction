train_data1 <- train_data[[1]]
test_data1 <- test_data[[1]]

# ---- Regression Trees ----

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

# Visualization
printcp(default_tree)
print(head(full_tree$cptable, 10))
plotcp(full_tree)


# Pruning
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

# Evaluation
train_metrics_full <- metrics(train_data1$baseRent, predict(full_tree, train_data1))
test_metrics_full <- metrics(test_data1$baseRent, predict(full_tree, test_data1))
train_metrics_default <- metrics(train_data1$baseRent, predict(default_tree, train_data1))
test_metrics_default <- metrics(test_data1$baseRent, predict(default_tree, test_data1))
train_metrics_pruned <- metrics(train_data1$baseRent, predict(pruned_tree, train_data1))
test_metrics_pruned <- metrics(test_data1$baseRent, predict(pruned_tree, test_data1))

# ---- SVMs ----

# best found model was 
# Metrics (Testing): MSE = 122979.3 MAE = 173.5774 
#best_svm <- svm(baseRent ~ ., data = train_data1, coef0=5, kernel = "polynomial")
best_svm <- svm(baseRent ~ ., data = train_data1, cost=5, kernel = "linear")

# tested for all kernels: "polynomial", "radial", "sigmoid", "linear"
tuned_svm <- tune(
  svm,
  baseRent ~ .,
  data = train_data1,
  ranges = list(cost = c(0.5,1,2,4), gamma = c(0.01, 0.5, 1, 2)) 
  # didnt specify all kernels at once for cross-validation with fear of using unnessecary vars like coef0 in radial 
  # for polynomial and sigmoid coef0=c(0,1,5,10)
  # for polynomial degree=c(2,3,6,10)
)

cv_best_svm <- tuned_svm$best.model


# Evaluation
train_pred_best <- predict(default_svm, train_data1)
test_pred_best <- predict(default_svm, test_data1)
train_pred_cv <- predict(best_svm, train_data1)
test_pred_cv <- predict(best_svm, test_data1)

train_metrics_best <- metrics(train_data1$baseRent, train_pred_best)
test_metrics_best <- metrics(test_data1$baseRent, test_pred_best)
train_metrics_cv <- metrics(train_data1$baseRent, train_pred_cv)
test_metrics_cv <- metrics(test_data1$baseRent, test_pred_cv)

train_pred_cv <- predict(cv_best_svm, train_data1)
test_pred_cv <- predict(cv_best_svm, test_data1)

# Step 2: Create a scatter plot
ggplot(train_data1, aes(x = heatingCosts, y = baseRent)) +
  geom_point(color = 'blue', alpha = 0.5) +  # Scatter plot of the original data
  geom_line(aes(x = heatingCosts, y = train_pred_best), color = 'red', size = 0.2) +  # SVM regression line
  labs(title = "Regression Line: Year Constructed vs Base Rent",
       x = "Year Constructed",
       y = "Base Rent") +
  theme_minimal()

test_metrics_best


# ---- GBM ----
