source("code/setup.R")

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
best_svm <- svm(baseRent ~ ., data = train_data1, coef0=5, kernel = "polynomial")

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


# ---- Randomforests ----
# cant use categorical variables with more than 53 categories in randomForest in R
train_data_rf <- train_data1 %>%
  select(where(~ !(is.factor(.) || is.character(.)) || n_distinct(.) <= 53))

train_control <- trainControl(method = "cv", number = 10)

tune_grid <- expand.grid(
  mtry = c(5, 10, 20)
)

tuned_rf <- train(
  baseRent ~ ., 
  data = train_data_rf, 
  method = "rf", 
  trControl = train_control, 
  tuneGrid = tune_grid,
  # cant include these params in grid, they also only lead to slight changes
  ntree=500 #, # tested for (200, 500, 1000)
 # nodesize=5     # c(2, 5, 10, 20)
)

tuned_rf <- tuned_rf$finalModel
tuned_rf

# 20, 500, node = 5, mtry = 20
best_rf <- randomForest(
  baseRent ~ ., 
  data = train_data_rf,
  ntree=500,
  mtry=20,
  nodesize=2
)

importance(best_rf)
varImpPlot(best_rf)

# Make predictions on training and testing datasets
train_pred_default <- predict(best_rf, train_data_new)
test_pred_default <- predict(best_rf, newdata = test_data1)

# Calculate metrics for training and testing predictions (using default MSE and MAE)
train_metrics_default <- postResample(train_pred_default, train_data1$baseRent)
test_metrics_default <- postResample(test_pred_default, test_data1$baseRent)


# ---- GBM ----
train_data_bool_as_factor <- train_data1 %>%
  mutate(across(where(is.logical), as.factor))

test_data_bool_as_factor <- test_data1 %>%
  mutate(across(where(is.logical), as.factor))

train_control <- trainControl(method = "cv",number = 10)

tuned_gbm <- train(
  baseRent ~ ., 
  data = train_data_bool_as_factor, 
  method = "gbm", 
  trControl = train_control,
  tuneGrid = expand.grid(
    n.trees = c(100, 200, 300, 500),    # Number of trees
    interaction.depth = c(1, 2, 3),     # Tree depth
    shrinkage = c(0.05, 0.1),           # Learning rate
    n.minobsinnode = c(10, 20)          # Minimum observations in a node
  ),
  verbose = FALSE
)

tuned_gbm <- tuned_gbm$finalModel
tuned_gbm

# Best GBM model params
#   n.trees interaction.depth shrinkage n.minobsinnode
#20     500                 3      0.05             10

best_gbm <- gbm(
  baseRent ~ .,
  data = train_data_bool_as_factor,
  distribution = "gaussian", #laplace to optimize MAE # For regression
  n.trees = 500,            
  interaction.depth = 6,     
  shrinkage = 0.05,        
  n.minobsinnode = 10       
)

# Importance Plot
importance <- summary(best_gbm, plotit = FALSE)

ggplot(importance, aes(x = reorder(var, rel.inf), y = rel.inf)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Variable Importance Plot",
    x = "Variables",
    y = "Relative Influence"
  ) +
  theme_minimal()

# Predictions for best models
train_pred_best <- predict(best_gbm, newdata = train_data_bool_as_factor)
test_pred_best <- predict(best_gbm, newdata = test_data_bool_as_factor)

train_metrics_best <- metrics(train_data1$baseRent, train_pred_best)
test_metrics_best <- metrics(test_data1$baseRent, test_pred_best)


# Display metrics
cat("Default GBM Metrics (Training): MSE =", train_metrics_best["MSE"], 
    "MAE =", train_metrics_best["MAE"], "\n")
cat("Default GBM Metrics (Testing): MSE =", test_metrics_best["MSE"], 
    "MAE =", test_metrics_best["MAE"], "\n")

pdp_data <- plot(best_gbm, i = "livingSpace", return.grid = TRUE)

ggplot(pdp_data, aes(x = livingSpace, y = y)) +
  geom_line() +
  labs(
    title = "Partial Dependence Plot for Living Space",
    x = "Living Space",
    y = "Base Rent"
  ) +
  theme_minimal()

# ---- XGBoost ----
# Perform One Hot Encoding Because xgb cant deal with factors on its own
dummies <- dummyVars(baseRent ~ ., data = train_data1)
train_encoded <- data.frame(predict(dummies, newdata = train_data1))
test_encoded <- data.frame(predict(dummies, newdata = test_data1))

train_matrix <- xgb.DMatrix(data = as.matrix(train_encoded), label = train_data1$baseRent)
test_matrix <- xgb.DMatrix(data = as.matrix(test_encoded), label = test_data1$baseRent)

# Random Grid Search CV
grid <- expand.grid(
  eta = c(0.01, 0.05, 0.1, 0.3, 0.5),      # Learning rates
  max_depth = c(1, 3, 6, 9),               # Tree depths
  min_child_weight = c(1, 2, 5, 10),       # Minimum child weight
  subsample = c(0.6, 0.8, 1.0),            # Row sampling ratios
  colsample_bytree = c(0.6, 0.8, 1.0)      # Feature sampling ratios
)
set.seed(2024)
grid_sample <- grid[sample(nrow(grid), size = 100), ]

results <- list()
for (i in 1:nrow(grid_sample)) {
  params <- list(
    objective = "reg:squarederror", 
    booster = "gbtree",
    eta = grid$eta[i],
    max_depth = grid$max_depth[i],
    min_child_weight = grid$min_child_weight[i],
    subsample = grid$subsample[i],
    colsample_bytree = grid$colsample_bytree[i]
  )
  
  cv_results <- xgb.cv(
    params = params,
    data = train_matrix,
    nrounds = 1000,           
    nfold = 10,              
    metrics = "rmse",                 
    early_stopping_rounds = 10,
    verbose = TRUE,
    nthread = 10
  )
  
  results[[i]] <- list(
    params = params,
    best_iteration = cv_results$best_iteration,
    rmse = min(cv_results$evaluation_log$test_rmse_mean)
  )
}

best_result <- results[[which.min(sapply(results, function(x) x$rmse))]]
best_params <- best_result$params
best_nrounds <- best_result$best_iteration

# Train model with best params
xgb_model_best <- xgboost(
  data = train_matrix,
  objective = "reg:squarederror",
  nrounds = 1000, # we dont need to use best rounds, uses best rounds by default 
  eta = 0.1,
  max_depth=6,
  min_child_weight=2,
  subsample=0.8,
  colsample_bytree=0.6,
  verbose = FALSE
)

# Evaluation
importance_matrix <- xgb.importance(model = xgb_model_best)

## Transform back to column representation
processed_importance <- importance_matrix %>%
  mutate(Feature = str_remove(Feature, "\\..*"),
         Feature = str_remove(Feature, "TRUE|FALSE")) %>%
  group_by(Feature) %>% 
  summarize(across(Gain:Importance, sum))

## Plot
print(processed_importance)
ggplot(processed_importance, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Variable Importance Plot",
    x = "Variables",
    y = "Relative Influence"
  ) +
  theme_minimal()
# worse than ggplot but native
# xgb.plot.importance(importance_matrix = importance_matrix[1:20])

train_pred <- predict(final_model, train_matrix)
test_pred <- predict(final_model, test_matrix)

train_metrics <- metrics(train_data1$baseRent, train_pred)
test_metrics <- metrics(test_data1$baseRent, test_pred)

