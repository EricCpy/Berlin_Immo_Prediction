berlin_data_cleaned <-readRDS("data/immo_data_berlin.rds")
berlin_data_cleaned <- berlin_data_cleaned %>%
  select(
    -regio1,                     # Remove "regio1" column - contains only "berlin", redundant information.
    -geo_bln,                    # Remove "geo_bln" column - contains only "berlin", redundant information.
    -geo_krs,                    # Remove "geo_krs" column - contains only "berlin", redundant information.  
    -regio2,                     # Remove "regio2" column - contains only "berlin", redundant information.
    -street,                     # Remove "street" column - contains only same information as street_plain but in worse quality.
    -telekomTvOffer,             # Remove "telekomTvOffer" column - everyone can get a telekomTvOffer if he wants to.
    -telekomHybridUploadSpeed,   # Remove "telekomHybridUploadSpeed" column - unnecessary; mostly NA values.
    -scoutId,                    # Remove "scoutId" column - unique ID, not useful for analysis.
    -yearConstructedRange,       # Remove "yearConstructedRange" column - artificial column, not relevant.
    -houseNumber,                # Remove "houseNumber" column - too granular, not relevant for analysis.
    -baseRentRange,              # Remove "baseRentRange" column - artificial column, not needed.
    -livingSpaceRange,           # Remove "livingSpaceRange" column - artificial column, not needed.
    -noRoomsRange,               # Remove "noRoomsRange" column - artificial column, not needed.
    -electricityKwhPrice,        # Remove "electricityKwhPrice" column - deprecated since 2020, irrelevant we dont have earlier dates.
    -electricityBasePrice,       # Remove "electricityBasePrice" column - deprecated since 2020, irrelevant.
    -energyEfficiencyClass,      # Remove "energyEfficiencyClass" column - deprecated since 2020, irrelevant.
    -date,                       # Remove "date" column - same as date_full
    -description,                # Remove "description" only contains information already provided by other columns as text
    -facilities                  # Remove "facilities" only contains information already provided by other columns as text
  ) %>%
  mutate(
    geo_plz = as.character(geo_plz)
  )

# if we are only allowed to use max 2000 obversations:
sub_df <- berlin_data_cleaned |>
  filter(regio3 %in% c("Mitte_Mitte", "Tiergarten_Tiergarten", "Charlottenburg_Charlottenburg"))

# sample 2000 observations from the sub_df
sample_df <- berlin_data_cleaned |>
  sample_n(2000)

