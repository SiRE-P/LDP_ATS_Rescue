#### Combining of all target data file into one and creating summary tables 
#### to look if two surveys were done on the same day######
library(dplyr)

### all of the CSVs I want to join
files <- c(
  "TARGET00_clean_V3.csv",
  "TARGET01_clean_V3.csv",
  "TARGET02_clean_V3.csv",
  "TARGET03_clean_V3.csv",
  "TARGET04_clean_V3.csv",
  "TARGET05_clean_V3.csv",
  "TARGET06_clean_V3.csv",
  "TARGET07_clean_V3.csv",
  "TARGET77_clean_V3.csv",
  "TARGET78_clean_V3.csv",
  "TARGET79_clean_V3.csv",
  "TARGET80_clean_V3.csv",
  "TARGET81_clean_V3.csv",
  "TARGET82_clean_V3.csv",
  "TARGET83_clean_V3.csv",
  "TARGET84_clean_V3.csv",
  "TARGET85_clean_V3.csv",
  "TARGET86_clean_V3.csv",
  "TARGET87_clean_V3.csv",
  "TARGET88_clean_V3.csv",
  "TARGET89_clean_V3.csv",
  "TARGET90_clean_V3.csv",
  "TARGET91_clean_V3.csv",
  "TARGET92_clean_V3.csv",
  "TARGET93_clean_V3.csv",
  "TARGET94_clean_V3.csv",
  "TARGET95_clean_V3.csv",
  "TARGET96_clean_V3.csv",
  "TARGET97_clean_V3.csv",
  "TARGET98_clean_V3.csv",
  "TARGET99_clean_V3.csv")

### combine them into one data frame renaming columns to add their respective units 
combined_data <- do.call(rbind, lapply(files, read.csv, stringsAsFactors = FALSE))

combined_data <- combined_data %>%
  rename(depth_min_m = depth_min)

combined_data <- combined_data %>%
  rename(depth_max_m = depth_max)

combined_data <- combined_data %>%
  rename(transect_length_m = transect_length)

combined_data <- combined_data %>%
  rename(area_ha = area)

### writing CSV file
write.csv(combined_data, "TARGET_1977_2007_combined.csv", row.names = FALSE)



### creating a table showing if there were more than one survey on the same date
library(dplyr)
library(tidyr)

### load data
data <- read.csv("TARGET07_ckean_V3.csv") 

lake_counts_by_date <- data %>%
  group_by(survey_date) %>%
  summarise(
    unique_lake_codes = n_distinct(lake_code),
    .groups = "drop"
  ) %>%
  arrange(desc(unique_lake_codes)) 

### view the result
print(lake_counts_by_date)
view(lake_counts_by_date)

write.csv(lake_counts_by_date, "TARGET07_survey_counts_by_date.csv", row.names = FALSE)


##### END OF SCRIPT #######
