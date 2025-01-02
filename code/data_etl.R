# ----- Berlin Data -----
berlin_data_semi_cleaned <-readRDS("data/immo_data_berlin.rds")
berlin_data_cleaned <- berlin_data_semi_cleaned %>%
  select(
    -regio1,                     # Remove "regio1" - contains only "berlin", redundant information.
    -geo_bln,                    # Remove "geo_bln" - contains only "berlin", redundant information.
    -geo_krs,                    # Remove "geo_krs" - contains only "berlin", redundant information.  
    -regio2,                     # Remove "regio2" - contains only "berlin", redundant information.
    -street,                     # Remove "street" - contains only same information as street_plain but in worse quality.
    -telekomTvOffer,             # Remove "telekomTvOffer" - everyone can get a telekomTvOffer if he wants to.
    -telekomHybridUploadSpeed,   # Remove "telekomHybridUploadSpeed" column - unnecessary; mostly NA values.
    -scoutId,                    # Remove "scoutId" - unique ID, not useful for analysis.
    -yearConstructedRange,       # Remove "yearConstructedRange" column - artificial column, not relevant.
    -houseNumber,                # Remove "houseNumber" - too granular, not relevant for analysis.
    -baseRentRange,              # Remove "baseRentRange" - artificial column, not needed.
    -livingSpaceRange,           # Remove "livingSpaceRange" - artificial column, not needed.
    -noRoomsRange,               # Remove "noRoomsRange" - artificial column, not needed.
    -electricityKwhPrice,        # Remove "electricityKwhPrice" - deprecated since 2020, irrelevant we dont have earlier dates.
    -electricityBasePrice,       # Remove "electricityBasePrice" - deprecated since 2020, irrelevant.
    -energyEfficiencyClass,      # Remove "energyEfficiencyClass" - deprecated since 2020, irrelevant.
    -date,                       # Remove "date" column - same as date_full
    -description,                # Remove "description" - only contains information already provided by other columns as text
    -facilities,                 # Remove "facilities" - only contains information already provided by other columns as text
    -totalRent,                  # Remove "totalRent" - artificial column, sum of baseRent, heatingCosts and serviceCharge 
    -newlyConst,                 # Remove "newlyConst" - artificial column 
    -streetPlain                 # Remove "streetPlain" - would require further cleaning or convertion into longitude and latitude
  ) %>%
  mutate(
    geo_plz = as.character(geo_plz)
  ) %>%
  distinct()

postal_data <- read.csv("data/postalcodes_with_districts_berlin.csv") %>%
  mutate(Postcode = as.character(Postcode)) %>%
  rename(district = District) %>%
  distinct(Postcode, .keep_all = TRUE)

bezirke_name_id <- tribble(
  ~DISTRICT_NAME, ~DISTRICT_ID,
  "Mitte", "01",
  "Friedrichshain-Kreuzberg", "02",
  "Pankow", "03",
  "Charlottenburg-Wilmersdorf", "04",
  "Spandau", "05",
  "Steglitz-Zehlendorf", "06",
  "Tempelhof-Schöneberg", "07",
  "Neukölln", "08",
  "Treptow-Köpenick", "09",
  "Marzahn-Hellersdorf", "10",
  "Lichtenberg", "11",
  "Reinickendorf", "12",
)

berlin_data_cleaned <- berlin_data_cleaned %>%
  left_join(postal_data, by = c("geo_plz" = "Postcode")) %>%
  left_join(bezirke_name_id, by = c("district" = "DISTRICT_NAME"))
  
# we should use ~2000 obversations based on the task description:
sub_df <- berlin_data_cleaned |>
  filter(regio3 %in% c("Mitte_Mitte", "Tiergarten_Tiergarten", "Charlottenburg_Charlottenburg"))

# ----- Geodata -----

berlin_district_geo <- sf::st_read(dsn = "data/berlin_lor") %>% 
  st_make_valid(tol = 0.00001) %>% st_transform(4326) %>% 
  mutate(
    DISTRICT_ID = str_sub(PLR_ID, 1, 2)
  ) %>% 
  left_join(bezirke_name_id, by = c("DISTRICT_ID" = "DISTRICT_ID")) %>%
  group_by(DISTRICT_ID, DISTRICT_NAME) %>% 
  summarize(
    geometry = st_union(geometry),
    .groups = 'drop'
  )


# ---- Train/Test Data ----
berlin_data_for_model <- berlin_data_cleaned %>%
  select(-DISTRICT_ID,
         -geo_plz) %>% # contains 214 levels on 10000 rows, likely that we get plzs in test data which arent in train data, 
                      # we also did some sort of dimension reduction on the geo_plzs by adding the districts and have subdistricts inside the regio3
  mutate(across(where(is.character), as.factor))

generate_gibbs <- FALSE
if (file.exists("./data/gibbs_berlin_data_model.rds")) {
  gibbs_berlin_data_for_model <- readRDS("./data/gibbs_berlin_data_model.rds")
  train_test_results <- generate_train_test_data_from_gibbs(gibbs_berlin_data_for_model)
  train_data <- train_test_results$train_data
  test_data <- train_test_results$test_data
  
} else if (generate_gibbs) {
  gibbs_berlin_data_for_model <- mice(berlin_data_for_model, m = 5, maxit = 50, meth = 'pmm', seed = 600)
  saveRDS(gibbs_immo, "./data/gibbs_berlin_data_model.rds")
  train_test_results <- generate_train_test_data_from_gibbs(gibbs_berlin_data_for_model)
  train_data <- train_test_results$train_data
  test_data <- train_test_results$test_data
  
} else {
  warning("No Gibbs data loaded or generated. Please ensure the RDS file exists or set generate_gibbs = TRUE in data_etl.")
}