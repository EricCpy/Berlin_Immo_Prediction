# gibbs-daten laden
source("code/setup.R")
library(here)

# 5 verschiedene training und test datensets
#train_data1 <- train_data[[1]]

# predicttor function und model functions speichern
#testfnc <- function(train_data){
#  svm(baseRent ~ .)
#}

### Filtering out outliers from the data

# boxplot for baseRent
ggplot(berlin_data_cleaned, aes(x = baseRent)) + 
  geom_boxplot() + 
  labs(title = "Distribution of Base Rent", x = "Base Rent", y = "Frequency")

# get upper bound
iqr <- IQR(berlin_data_cleaned$baseRent)
q3 <- quantile(berlin_data_cleaned$baseRent, 0.75)
upper_bound <- unname(q3 + (1.5 * iqr))

# filter data for upper_bound
test_gibbs <- complete(gibbs_berlin_data_for_model, 1)
test_gibbs <- test_gibbs |>
  filter(baseRent < upper_bound)
set.seed(123)
train_fraction <- 0.8
train_index <- sample(seq_len(nrow(test_gibbs)), size = train_fraction * nrow(test_gibbs))
train_data_no_outliers <- test_gibbs[train_index, ]
test_data_no_outliers <- test_gibbs[-train_index, ]

train_data_bool_as_factor <-  train_data_no_outliers |>
  mutate(across(where(is.logical), as.factor))
test_data_bool_as_factor <-  test_data_no_outliers |>
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

# svm
model <- svm(baseRent ~ ., data = train_data_bool_as_factor, coef=5, kernel = "polynomial")

O
mse <- mean((test_data_bool_as_factor$baseRent - predictions)^2)
rmse <- sqrt(mse)
mae <- mean(abs(test_data_bool_as_factor$baseRent - predictions))

saveRDS(model, here("data/svm_model_baseRent_no_outliers.rds"))
