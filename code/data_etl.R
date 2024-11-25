immo_data <- read.csv("data/immo_data.csv")
names(immo_data)
berlin_data <- immo_data %>%
  filter(regio1 == "Berlin")
summary(berlin_data) # ~10000rows

saveRDS(berlin_data, file = "data/immo_data_berlin.rds")
berlin_data <- readRDS("data/immo_data_berlin.rds")
