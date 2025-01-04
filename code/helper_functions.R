metrics <- function(actual, predicted) {
  mse <- mean((actual - predicted)^2)
  mae <- mean(abs(actual - predicted))
  return(c(MSE = mse, RMSE = sqrt(mse), MAE = mae))
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


train_and_evaluate_models <- function(train_data, test_data, train_model_function, predict_function, saved_models_path = "", save_model = FALSE) {
  mse_list <- c()
  rmse_list <- c()
  mae_list <- c()
  
  if (file.exists(saved_models_path)) {
    loaded_models <- readRDS(saved_models_path)
  } else {
    loaded_models <- list()
  }
  
  for (i in 1:length(train_data)) {
    train_data_i <- train_data[[i]]
    test_data_i <- test_data[[i]]
    
    if (length(loaded_models) >= i) {
      model <- loaded_models[[i]]
    } else {
      model <- train_model_function(train_data_i)
      if (save_model) {
        loaded_models[[i]] <- model
      }
    }
    
    predictions <- predict_function(model, test_data_i)
    mse <- mean((test_data_i$baseRent - predictions)^2)
    rmse <- sqrt(mse)
    mae <- mean(abs(test_data_i$baseRent - predictions))
    
    mse_list <- c(mse_list, mse)
    rmse_list <- c(rmse_list, rmse)
    mae_list <- c(mae_list, mae)
  }
  
  if (save_model) {
    saveRDS(loaded_models, saved_models_path)
  }

  return(list(
    mse_all = mse_list,
    rmse_all = rmse_list,
    mae_all = mae_list,
    average_mse = mean(mse_list),
    average_rmse = mean(rmse_list),
    average_mae = mean(mae_list)
  ))
}
