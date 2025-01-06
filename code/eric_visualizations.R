source("code/setup.R")

# View the cleaned dataframe
head(sub_df)
sum(is.na(berlin_data_cleaned$totalRent))
colnames(berlin_data_cleaned)


# ---- Visualizations Preparation ----

berlin_data_cleaned_districts_visualizations <- berlin_data_cleaned  %>%
  filter(!is.na(district)) # only 30 NA Rows

berlin_data_cleaned$district_label <- paste(berlin_data_cleaned$DISTRICT_ID, berlin_data_cleaned$district, sep = " - ")
berlin_data_cleaned$log_baseRent <- log(berlin_data_cleaned$baseRent)

berlin_data_district_statistics <- berlin_data_cleaned %>%
  group_by(district, district_label) %>% 
  summarize(count_apartments = n(),
            median_base_rent = median(baseRent, na.rm = TRUE),
            , .groups = 'drop') %>%
  left_join(berlin_district_geo, by = c("district" = "DISTRICT_NAME")) %>%
  filter(!is.na(district))

# ---- Visualization of the Berlin map count of apartments in districts ----

ggplot() +
  geom_sf(data = berlin_data_district_statistics, aes(geometry = geometry, fill = count_apartments)) +
  geom_sf_text(data = berlin_data_district_statistics, aes(geometry = geometry, label = DISTRICT_ID), size = 3, color = "black") +
  scale_fill_gradient(low = "lightblue", high = "dodgerblue4", name = "Apartment Count") +
  theme_void() +
  labs(title = "Apartment Counts by District in Berlin") +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5)) +
  guides(fill = "none")

ggplot(berlin_data_district_statistics, aes(x = reorder(district_label, count_apartments), y = count_apartments, fill = count_apartments)) +
  geom_col() +
  scale_fill_gradient(low = "lightblue", high = "dodgerblue4", name = "Apartment Count") +
  labs(title = "Apartment Counts by District in Berlin", x = "District", y = "Apartment Count") +
  theme_minimal() +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = 0.5)) +
  guides(fill = "none")


# ---- Visulizations of berlin map median price of districts ----
a <- ggplot() +
  geom_sf(data = berlin_data_district_statistics, aes(geometry = geometry, fill = median_base_rent)) +
  geom_sf_text(data = berlin_data_district_statistics, aes(geometry = geometry, label = DISTRICT_ID), size = 3, color = "black") +
  scale_fill_gradient(low = "lightyellow", high = "darkorange", name = "Median Rent") +
  theme_void() +
  labs(title = "Median Price by District in Berlin") +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5)) +
  guides(fill = "none")

berlin_data_cleaned_log <- berlin_data_cleaned %>%
  filter(!is.na(log_baseRent) & is.finite(log_baseRent)) %>%
  left_join(berlin_data_district_statistics, by = "district_label")

# boxplot based on median rent
b <- ggplot(berlin_data_cleaned_log, aes(x = reorder(district_label, log_baseRent, FUN = median), 
                                    y = log_baseRent, fill = median_base_rent)) +
  geom_boxplot() +
  scale_fill_gradient(low = "lightyellow", high = "darkorange", name = "Median Rent") +
  labs(title = "Base Rent Distribution by District in Berlin", 
       x = "District", y = "Log Base Rent") +
  theme_minimal() +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = 0.5)) +
  guides(fill = "none")

grid.arrange(a, b, ncol=2)

# ---- Compare living space with number of rooms ----
berlin_data_cleaned$noRoomsRounded <- round(berlin_data_cleaned$noRooms)

ggplot(berlin_data_cleaned, aes(x = factor(noRoomsRounded))) +
  geom_bar(fill = "steelblue", alpha = 0.7) +
  labs(title = "Count of Properties by Number of Rooms",
       x = "Number of Rounded Rooms",
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

iqr_living_space <- IQR(berlin_data_cleaned_log$livingSpace, na.rm = TRUE)
iqr_rooms <- IQR(berlin_data_cleaned_log$noRooms, na.rm = TRUE)

# Calculate the lower and upper bounds for living space and number of rooms
lower_living_space <- quantile(berlin_data_cleaned_log$livingSpace, 0.25) - 1.5 * iqr_living_space
upper_living_space <- quantile(berlin_data_cleaned_log$livingSpace, 0.75) + 1.5 * iqr_living_space
lower_rooms <- quantile(berlin_data_cleaned_log$noRooms, 0.25) - 1.5 * iqr_rooms
upper_rooms <- quantile(berlin_data_cleaned_log$noRooms, 0.75) + 1.5 * iqr_rooms

# Filter out outliers
berlin_data_no_outliers <- berlin_data_cleaned_log %>%
  filter(noRooms <= 8.49)

berlin_data_no_outliers$noRoomsRounded <- round(berlin_data_no_outliers$noRooms)

# Boxplot
ggplot(berlin_data_no_outliers, aes(x = factor(noRoomsRounded), y = livingSpace)) +
  geom_boxplot(aes(fill = factor(noRoomsRounded)), alpha = 0.7) +
  labs(title = "Living Space Distribution by Number of Rooms", 
       x = "Number of Rooms", 
       y = "Living Space (Square Meters)") +
  scale_fill_brewer(palette = "Blues") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# ---- Boxplot for baseRent by condition ----
ggplot(berlin_data_cleaned, aes(x = reorder(condition, baseRent, FUN = median), y = baseRent, fill = condition)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Blues") +
  scale_y_log10() +
  coord_flip() +
  labs(title = "Total Rent by Condition", x = "Condition", y = "Total Rent (EUR)") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10)) +
  guides(fill = "none")


# ---- Correlation plot baseRent ----
numeric_vars <- berlin_data_cleaned %>% select_if(is.numeric)
cor_matrix <- cor(numeric_vars,  use = "complete.obs")
corrplot::corrplot(cor_matrix, method = "circle", type = "lower", 
                   tl.cex = 0.8, 
                   title = "Correlation Matrix (BaseRent)", 
                   mar = c(0, 0, 1, 0))

base_rent_corr <- cor_matrix["baseRent", ]
cor_data <- data.frame(variable = names(base_rent_corr), correlation = base_rent_corr)
ggplot(cor_data, aes(x = reorder(variable, correlation), y = correlation)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Correlation with baseRent", x = "Variables", y = "Correlation Coefficient") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10), axis.text.x = element_text(size = 10))


# ---- Missings ----
top_na_counts <- berlin_data_cleaned %>%
  summarise(across(everything(), ~ sum(is.na(.)), .names = "{col}")) %>%
  pivot_longer(cols = everything(), names_to = "Column", values_to = "MissingCount") %>%
  arrange(desc(MissingCount)) %>%
  slice_head(n = 10)

ggplot(top_na_counts, aes(x = reorder(Column, MissingCount), y = MissingCount)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Top 10 Columns with Most Missing Values",
    x = "Columns",
    y = "Count of Missing Values"
  ) +
  theme_minimal() +
  coord_flip()


# ---- Regression Line SVM ----

test_data1 <- test_data[[1]]

svm_model1 <- readRDS("data/models/models_svm.rds")[1]

train_data1 <- train_data[[1]]
train_pred_svm <- predict(svm_model1, train_data1)[[1]]

ggplot(train_data1, aes(x = livingSpace, y = baseRent)) +
  geom_point(color = 'blue', alpha = 0.5) +  # Scatter plot of the original data
  geom_line(aes(x = livingSpace, y = train_pred_svm), color = 'red', size = 0.2) +  # SVM regression line
  labs(title = "Regression Line: Year Constructed vs Base Rent",
       x = "Year Constructed",
       y = "Base Rent") +
  theme_minimal()

# ---- XGB TRUE VS PREDICTED ----
test_data1 <- test_data[[1]]

dummies <- dummyVars(baseRent ~ ., data = test_data1)
test_encoded <- data.frame(predict(dummies, newdata = test_data1))
test_matrix <- xgb.DMatrix(data = as.matrix(test_encoded), label = test_data1$baseRent)

xgb_model_best <- readRDS("data/models/models_xgb.rds")[1]

test_pred <- predict(xgb_model_best, test_matrix)[[1]]

results <- data.frame(
  True = test_data1$baseRent,
  Predicted = test_pred
)

# Create the ggplot
ggplot(results, aes(x = True, y = Predicted)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(
    x = "True Values (baseRent)",
    y = "Predicted Values (baseRent)"
  )
