metrics <- function(actual, predicted) {
  mse <- mean((actual - predicted)^2)
  mae <- mean(abs(actual - predicted))
  return(c(MSE = mse, MAE = mae))
}

generate_train_test_data_from_gibbs <- function(imputed_model, seed = 123, train_fraction = 0.8) {
  train_data <- list()
  test_data <- list()
  
  for (i in 1:imputed_model$m) {
    imputed_data <- complete(imputed_model, i)
    set.seed(seed)
    train_index <- sample(seq_len(nrow(imputed_data)), size = train_fraction * nrow(imputed_data))
    train_data[[i]] <- imputed_data[train_index, ]
    test_data[[i]] <- imputed_data[-train_index, ]
  }
  
  return(list(train_data = train_data, test_data = test_data))
}