library(tidyverse)

immo_df <- readRDS("data/immo_data_berlin.rds")

table(immo_df$date)

# prepare dates properly
lookup_table <- data.frame(
  date = c("Feb20", "May19", "Oct19", "Sep18"),
  date_full = c("20.02.2020", "19.05.2020", "19.10.2020", "18.09.2020")
)

immo_df <- immo_df |>
  left_join(lookup_table, by = "date")

# transform the full date to datetime format
immo_df$date_full <- dmy(immo_df$date_full)

# looking up districts
freq_table <- table(immo_df$regio3)

# Sort the table by frequency in descending order
sorted_freq_table <- sort(freq_table, decreasing = TRUE)
sorted_freq_table

cum_freq_table <- cumsum(sorted_freq_table)
cum_freq_table

table(immo_df$geo_bln)
table(immo_df$geo_krs)
table(immo_df$regio2)
table(immo_df$regio1)

# delete regio2, geo_bln and geo_krs because they hold the same info as regio1
immo_df$geo_bln <- NULL
immo_df$geo_krs <- NULL
immo_df$regio2 <- NULL

# delete street as streetPlain contains the same info in better quality
immo_df$street <- NULL

# fix energyEfficiencyClass (NO_INFORMATION = NA)
table(immo_df$energyEfficiencyClass)
immo_df$energyEfficiencyClass[immo_df$energyEfficiencyClass == "NO_INFORMATION"] <- NA
table(immo_df$energyEfficiencyClass)

# I think to know that no offer is provided is better than setting it NA
table(immo_df$telekomTvOffer)

# transform scoutID to character
immo_df$scoutId <- as.character(immo_df$scoutId)

# I don't know if any of these classes are equivalent, energy specialists step forward
table(immo_df$firingTypes)

# WTF is "negotiable" as a condition???
table(immo_df$condition)

# transform plz to character as it's post id of district
immo_df$geo_plz <- as.character(immo_df$geo_plz)

# save cleaned but not subsetted df
saveRDS(immo_df,"data/cleaned_immo.rds")

sub_df <- immo_df |>
  filter(regio3 %in% c("Mitte_Mitte", "Tiergarten_Tiergarten", "Charlottenburg_Charlottenburg"))

# sample 2000 observations from the sub_df
sample_df <- sub_df |>
  sample_n(2000)

saveRDS(sample_df, "data/subset_by_district_immo.rds")