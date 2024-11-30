source("code/setup.R")

#immo_data <- read.csv("data/immo_data.csv")
#names(immo_data)
#berlin_data <- immo_data %>%
#  filter(regio1 == "Berlin")
#summary(berlin_data) # ~10000rows

#saveRDS(berlin_data, file = "data/immo_data_berlin.rds")
#berlin_data <- readRDS("data/immo_data_berlin.rds")
#names(immo_data)
#summary(berlin_data)

berlin_data_cleaned <-readRDS("data/cleaned_immo.rds")
names(berlin_data_cleaned)

berlin_data_cleaned <- berlin_data_cleaned %>%
  select(
    -regio1,                     # Remove "regio1" column - contains only "berlin", redundant information.
    -telekomHybridUploadSpeed,   # Remove "telekomHybridUploadSpeed" column - unnecessary; mostly NA values.
    -scoutId,                    # Remove "scoutId" column - unique ID, not useful for analysis.
    -geo_plz,                    # Remove "geo_plz" column - same as "regio1", redundant.
    -yearConstructedRange,       # Remove "yearConstructedRange" column - artificial column, not relevant.
    -houseNumber,                # Remove "houseNumber" column - too granular, not relevant for analysis.
    -baseRentRange,              # Remove "baseRentRange" column - artificial column, not needed.
    -livingSpaceRange,           # Remove "livingSpaceRange" column - artificial column, not needed.
    -noRoomsRange,               # Remove "noRoomsRange" column - artificial column, not needed.
    -electricityKwhPrice,        # Remove "electricityKwhPrice" column - deprecated since 2020, irrelevant we dont have earlier dates.
    -electricityBasePrice,       # Remove "electricityBasePrice" column - deprecated since 2020, irrelevant.
    -energyEfficiencyClass,      # Remove "energyEfficiencyClass" column - deprecated since 2020, irrelevant.
    -date                        # Remove "date" column - same as date_full
    -description,                # Remove "description" only contains information already provided by other columns as text
    -facilities                  # Remove "facilities" only contains information already provided by other columns as text
  )

# View the cleaned dataframe
head(berlin_data_cleaned %>%
       select(

       ))


sum(is.na(berlin_data_cleaned$totalRent))
