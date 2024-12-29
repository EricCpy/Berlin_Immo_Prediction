library(ggplot2)
library(dplyr)

#datasets
cleaned_immo <- readRDS("data/cleaned_immo.rds")
subset_immo <- readRDS("data/subset_by_district_immo.rds")

#the structure of the datasets
str(cleaned_immo)
summary(cleaned_immo)
str(subset_immo)
summary(subset_immo)

#Analyzing Numeric Variables
#compute correlations and visualize totalRent or baseRent against key variables like livingSpace
#correlation matrix
numeric_vars <- cleaned_immo %>% select_if(is.numeric)
cor_matrix <- cor(numeric_vars, use = "complete.obs")
cor_matrix["totalRent", ]  # Correlation with totalRent

#scatter plot for totalRent vs livingSpace
ggplot(cleaned_immo, aes(x = livingSpace, y = totalRent)) +
  geom_point(alpha = 0.5) +
  labs(title = "Living Space vs Total Rent", x = "Living Space (sqm)", y = "Total Rent (EUR)")


#Analyzing Categorical Variables
#how rent prices vary across districts or conditions
#boxplot for totalRent by district
ggplot(subset_immo, aes(x = regio3, y = totalRent)) +
  geom_boxplot() +
  coord_flip() +  # Flip for better readability
  labs(title = "Total Rent by District", x = "District", y = "Total Rent (EUR)")

#boxplot for totalRent by condition
ggplot(cleaned_immo, aes(x = condition, y = totalRent)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "Total Rent by Condition", x = "Condition", y = "Total Rent (EUR)")
