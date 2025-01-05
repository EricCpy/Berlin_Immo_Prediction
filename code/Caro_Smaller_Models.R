source("code/setup.R")
library(here)

# select variables

test_gibbs <- complete(gibbs_berlin_data_for_model, 1)

test_gibbs <- test_gibbs |>
  select(baseRent, heatingType, yearConstructed, hasKitchen, livingSpace, condition, interiorQual,
         typeOfFlat, noRooms, thermalChar, heatingCosts, district)

# train test split
set.seed(123)
train_fraction <- 0.8
train_index <- sample(seq_len(nrow(test_gibbs)), size = train_fraction * nrow(test_gibbs))
train_data_small <- test_gibbs[train_index, ]
test_data_small <- test_gibbs[-train_index, ]

train_data_bool_as_factor <-  train_data_small |>
  mutate(across(where(is.logical), as.factor))
test_data_bool_as_factor <-  test_data_small |>
  mutate(across(where(is.logical), as.factor))

# gbm
model <- gbm(baseRent ~ .,
             data = train_data_bool_as_factor,
             distribution = "gaussian",
             n.trees = 500,
             interaction.depth = 3,
             shrinkage = 0.05,
             n.minobsinnode = 10 
)

predictions <- predict(model, test_data_bool_as_factor)
mse <- mean((test_data_bool_as_factor$baseRent - predictions)^2)
rmse <- sqrt(mse)
mae <- mean(abs(test_data_bool_as_factor$baseRent - predictions))