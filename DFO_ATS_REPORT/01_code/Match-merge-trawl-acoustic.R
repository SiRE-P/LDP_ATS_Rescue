##############
####    ATS data                
####    summarizing Acoustic and trawl data       
####    Authors: Alice Assmar (McGill Uni.), David Hunt (McGill Uni.),
####    Howard Stiff (DFO Nanaimo), Athena Ogden (DFO Nanaimo)
###############

# getwd()
# setwd("./LDP_ATS_Rescue")

# Install necessary packages if they are not yet installed
packages <- c("beepr", "dplyr", "lubridate","progress",
              "purrr","stringr", "tibble", "tictoc", "tidyverse", "tools", "Rcpp", "fuzzyjoin")
install.packages(setdiff(packages, row.names(installed.packages())))

# Load all necessary packages
{
  library(beepr)
  library(dplyr)
  library(lubridate)
  library(purrr)
  library(stringr)
  library(tidyverse)
  library(tools)
  library(Rcpp)
  library(fuzzyjoin)
}

############## Step 1
############## Create a vector to hold the path to input and output files

data_directory <- "./00_data/"
final_directory <- "./02_final_output/"

############## Step 2  
############## Read csv tables

Acoustic_data <- read.csv(paste0(data_directory,"Target_OUTPUT_data_FINAL_(1977_2007).csv"))
Trawl_data <- read.csv(paste0(data_directory,"Trawl_data_FINAL_1977-1999.csv"))

############## Step 3 
############## Check the tables

#### Summarize the tables
summary(Acoustic_data)
summary(Trawl_data)

common_data <- intersect(names(Acoustic_data), names(Trawl_data))

# Remove abbreviations in the name of the lakes in the Acoustic dataset by importing the lookup table
lake_name <- read.csv("../TRAWL_BIOSAMPLE/00_raw_data/04_YS_look_up_tables/lake_codes.csv")

# Join the tables
Acoustic_data <- Acoustic_data %>%
  mutate(lake_code = case_when(lake_code == 124 ~ 180,
                               TRUE ~ lake_code)) %>%
  left_join(lake_name, by = "lake_code") %>%
  mutate(source_file = "Acoustic")

#### Rename headers of Acoustic_data and Trawl_data columns to match them
Trawl_data <- Trawl_data %>% 
  rename("survey_date" = trawl_date) %>%
  mutate(source_file = "Trawl")

#Acoustic_data <- Acoustic_data %>% 
#  rename("lake_name" = lake) %>%
#  mutate(source_file = "Acoustic dataset")

# How many dates the lake was surveyed
aggregate(survey_date ~ lake_code, data = Trawl_data, FUN = function(x) length(unique(x))) # n = 66
aggregate(survey_date ~ lake_code, data = Acoustic_data, FUN = function(x) length(unique(x))) # n = 82

# summarize each table
Trawl_data %>%
  group_by(ats_year, survey_date, lake_code, lake_name) %>%
  summarise(count_trawl = n()) %>%
  arrange(survey_date, lake_name) -> summary_table_trawl

Acoustic_data %>%
  group_by(ats_year, survey_date, lake_code, lake_name) %>%
  summarise(count_acoustic = n()) %>%
  arrange(survey_date, lake_name) -> summary_table_acoustic

# combine and compare results
summary_table_trawl_acoustic <-full_join(summary_table_trawl, summary_table_acoustic, 
                                   by=c("ats_year", "survey_date", "lake_code", "lake_name"))

summary_table_trawl_acoustic <- summary_table_trawl_acoustic %>%
  arrange(survey_date, lake_name) %>%
  mutate(survey_match = case_when(!is.na(count_trawl) & !is.na(count_acoustic) ~ "Match", 
                                  !is.na(count_trawl) & is.na(count_acoustic) ~ "Trawl only",
                                  TRUE ~ "Acoustic only"))

# Save final table in csv
write.csv(summary_table_trawl_acoustic, paste0(final_directory, "/", "Records_count_trawl_acoustic_by_lake_same_date.csv"), row.names = FALSE)

# How many dates the lake was surveyed and number of trawls
aggregate(cbind(survey_date, ats_year) ~ lake_code, data = Acoustic_data, FUN = function(x) length(unique(x)))
aggregate(cbind(survey_date, ats_year) ~ lake_code, data = Trawl_data, FUN = function(x) length(unique(x)))

# Check rows that matches in each dataset 
matched_data_trawl <- semi_join(Trawl_data, Acoustic_data, by = c("survey_date", "lake_name", "ats_year"))
matched_data_acoustic <- semi_join(Acoustic_data, Trawl_data, by = c("survey_date", "lake_name", "ats_year"))

# Check rows that did not match in each dataset 
mismatched_data_trawl <- anti_join(Trawl_data, Acoustic_data, by = c("survey_date", "lake_name", "ats_year"))
mismatched_data_acoustic <- anti_join(Acoustic_data, Trawl_data, by = c("survey_date", "lake_name", "ats_year"))

###
#### Create the intervals of two days for each unique survey date #####
###

# guarantee date are in date, not character
Trawl_data_date <- Trawl_data %>%
  mutate(survey_date = as.Date(survey_date),
         interval_start = survey_date - 2,
         interval_end = survey_date + 2)

Acoustic_data_date <- Acoustic_data %>%
  mutate(survey_date = as.Date(survey_date))
         
# create a table with the info I want to check
Trawl_data_date$lake_name <-gsub(" ", "_", Trawl_data_date$lake_name)
unique_Trawl_data <- tibble(unique = unique(paste(Trawl_data_date$lake_code, Trawl_data_date$lake_name, Trawl_data_date$survey_date, 
                                       Trawl_data_date$interval_start, Trawl_data_date$interval_end, Trawl_data_date$ats_year, 
                                       Trawl_data_date$source_file))) 
Acoustic_data_date$lake_name <-gsub(" ", "_", Acoustic_data_date$lake_name)
unique_acoustic_data <- tibble(unique = unique(paste(Acoustic_data_date$lake_code, Acoustic_data_date$lake_name, Acoustic_data_date$survey_date,
                                                     Acoustic_data_date$ats_year, Acoustic_data_date$source_file)))

unique_Trawl_data <- unique_Trawl_data %>%
  separate(col = unique, into = c("lake_code", "lake_name", "survey_date", "interval_start","interval_end", 
                                  "ats_year", "source_file"), sep = " ")

unique_acoustic_data <- unique_acoustic_data %>%
  separate(col = unique, into = c("lake_code","lake_name", "survey_date", "ats_year", "source_file"), sep = " ")

# line up identical rows
columns <- c("lake_code", "lake_name", "survey_date", "ats_year", "source_file")

# Check for leading/trailing whitespace
for (col in columns) {
  unique_acoustic_data[[col]] <- trimws(unique_acoustic_data[[col]])}

for (col in columns) {
  unique_Trawl_data[[col]] <- trimws(unique_Trawl_data[[col]])}

# Merge the database using the unique ID to check if the intervals worked
merged_df <- merge(unique_acoustic_data [, columns, drop = FALSE],
                   unique_Trawl_data [, c("interval_start", "interval_end", columns), drop = FALSE],
                   by = c("lake_name", "ats_year", "survey_date", "lake_code"),
                   all = TRUE, # Keep all
                   suffixes = c("_acoustic", "_trawl"))

###
#### Create a match/merge table based on the intervals of two days (allowing a fuzzy interval of 2 days) #####
###

# Set the class as date
unique_Trawl_data$survey_date <- as.Date(unique_Trawl_data$survey_date)
unique_acoustic_data$survey_date <- as.Date(unique_acoustic_data$survey_date)

# Add unique IDs for each row
unique_Trawl_data <- unique_Trawl_data %>%
  mutate(id_trawl = row_number())
unique_acoustic_data <- unique_acoustic_data %>%
  mutate(id_acoustic = row_number())

# Find valid matches
matches_survey_date_by_lake <- unique_Trawl_data %>%
  inner_join(unique_acoustic_data, by = c("lake_name", "ats_year", "lake_code"), suffix = c("_trawl", "_acoustic")) %>%
  mutate(date_diff = abs(as.numeric(survey_date_trawl - survey_date_acoustic))) %>%
  filter(date_diff <= 2) %>%
  mutate(match_status_acoustic = "MATCH")

# state any given date of the acoustic that match more than one interval of the trawl survey
date_repeated <- matches_survey_date_by_lake %>%
  count(lake_name, survey_date_acoustic, name = "count_acoustic_survey")

matches_survey_date_by_lake <- matches_survey_date_by_lake %>%
  left_join(date_repeated, by = c("lake_name", "survey_date_acoustic")) %>%
  mutate(overlap_status_acoustic = case_when(count_acoustic_survey != 1 ~ "Acoustic date matches more than one trawl date", 
                                 TRUE ~ "Acoustic date matches one trawl date"))

# Find unmatched id_trawl rows
unmatched_trawl <- unique_Trawl_data %>%
  filter(!id_trawl %in% matches_survey_date_by_lake$id_trawl) %>%
  rename("survey_date_trawl" = survey_date) %>%
  rename("source_file_trawl" = source_file) %>%
  mutate(match_status_acoustic = "MISMATCH")

# Find unmatched id_acoustic rows
unmatched_acoustic <- unique_acoustic_data %>%
  filter(!id_acoustic %in% matches_survey_date_by_lake$id_acoustic) %>%
  rename("survey_date_acoustic" = survey_date) %>%
  rename("source_file_acoustic" = source_file) %>%
  mutate(match_status_acoustic = "MISMATCH")

# Combine everything
df_final_merged <- bind_rows(matches_survey_date_by_lake, unmatched_trawl, unmatched_acoustic)

# Remove columns that are not needed
df_final_merged <- df_final_merged %>%
  unite(col = "source_file", source_file_trawl, source_file_acoustic, sep = ", ") %>%
  arrange(ats_year, lake_name) %>%
  select(ats_year, lake_code, lake_name, survey_date_trawl, interval_start, interval_end,
         survey_date_acoustic, source_file, match_status_acoustic, overlap_status_acoustic, count_acoustic_survey, 
         -id_trawl, -id_acoustic, -date_diff)

# Remove NAs that were added to the rows
replacement_pattern <- c("NA, " = "", ", NA" = "")
df_final_merged$source_file <- str_replace_all(df_final_merged$source_file, replacement_pattern)

# Save final table in csv
write.csv(df_final_merged, paste0(final_directory, "/", "FINAL_2days_match_trawl_and_acoustic.csv"), row.names = FALSE)


###
#### Create the intervals of SEVEN days for each unique survey date #####
###

# guarantee date are in date, not character
Trawl_data_seven_days <- Trawl_data %>%
  mutate(survey_date = as.Date(survey_date),
         interval_start = survey_date - 7,
         interval_end = survey_date + 7)

Acoustic_data_seven_days <- Acoustic_data %>%
  mutate(survey_date = as.Date(survey_date))

# create a table with the info I want to check
Trawl_data_seven_days$lake_name <-gsub(" ", "_", Trawl_data_seven_days$lake_name)
unique_Trawl_seven_days <- tibble(unique = unique(paste(Trawl_data_seven_days$lake_code, Trawl_data_seven_days$lake_name, Trawl_data_seven_days$survey_date, 
                                                  Trawl_data_seven_days$interval_start, Trawl_data_seven_days$interval_end, Trawl_data_seven_days$ats_year, 
                                                  Trawl_data_seven_days$source_file))) 
Acoustic_data_seven_days$lake_name <-gsub(" ", "_", Acoustic_data_seven_days$lake_name)
unique_acoustic_seven_days <- tibble(unique = unique(paste(Acoustic_data_seven_days$lake_code, Acoustic_data_seven_days$lake_name, Acoustic_data_seven_days$survey_date,
                                                     Acoustic_data_seven_days$ats_year, Acoustic_data_seven_days$source_file)))

unique_Trawl_seven_days <- unique_Trawl_seven_days %>%
  separate(col = unique, into = c("lake_code", "lake_name", "survey_date", "interval_start","interval_end", 
                                  "ats_year", "source_file"), sep = " ")

unique_acoustic_seven_days <- unique_acoustic_seven_days %>%
  separate(col = unique, into = c("lake_code","lake_name", "survey_date", "ats_year", "source_file"), sep = " ")

# line up identical rows
columns <- c("lake_code", "lake_name", "survey_date", "ats_year", "source_file")

# Check for leading/trailing whitespace
for (col in columns) {
  unique_acoustic_seven_days[[col]] <- trimws(unique_acoustic_seven_days[[col]])}

for (col in columns) {
  unique_Trawl_seven_days[[col]] <- trimws(unique_Trawl_seven_days[[col]])}

###
#### Create a match/merge table based on the intervals of SEVEN days (allowing a fuzzy interval of 7 days) #####
###

# Set the class as date
unique_Trawl_seven_days$survey_date <- as.Date(unique_Trawl_seven_days$survey_date)
unique_acoustic_seven_days$survey_date <- as.Date(unique_acoustic_seven_days$survey_date)

# Add unique IDs for each row
unique_Trawl_seven_days <- unique_Trawl_seven_days %>%
  mutate(id_trawl = row_number())
unique_acoustic_seven_days <- unique_acoustic_seven_days %>%
  mutate(id_acoustic = row_number())

# Find valid matches
matches_survey_date_by_lake_interval7 <- unique_Trawl_seven_days %>%
  inner_join(unique_acoustic_seven_days, by = c("lake_name", "ats_year", "lake_code"), suffix = c("_trawl", "_acoustic")) %>%
  mutate(date_diff = abs(as.numeric(survey_date_trawl - survey_date_acoustic))) %>%
  filter(date_diff <= 7) %>%
  mutate(match_status_acoustic = "MATCH")

# state any given date of the acoustic that match more than one interval of the trawl survey
date_repeated_interval7 <- matches_survey_date_by_lake_interval7 %>%
  count(lake_name, survey_date_acoustic, name = "count_acoustic_survey")

matches_survey_date_by_lake_interval7 <- matches_survey_date_by_lake_interval7 %>%
  left_join(date_repeated_interval7, by = c("lake_name", "survey_date_acoustic")) %>%
  mutate(overlap_status_acoustic = case_when(count_acoustic_survey != 1 ~ "Acoustic date matches more than one trawl date", 
                                             TRUE ~ "Acoustic date matches one trawl date"))

# Find unmatched id_trawl rows
unmatched_trawl_interval7 <- unique_Trawl_seven_days %>%
  filter(!id_trawl %in% matches_survey_date_by_lake_interval7$id_trawl) %>%
  rename("survey_date_trawl" = survey_date) %>%
  rename("source_file_trawl" = source_file) %>%
  mutate(match_status_acoustic = "MISMATCH")

# Find unmatched id_acoustic rows
unmatched_acoustic_interval7 <- unique_acoustic_seven_days %>%
  filter(!id_acoustic %in% matches_survey_date_by_lake_interval7$id_acoustic) %>%
  rename("survey_date_acoustic" = survey_date) %>%
  rename("source_file_acoustic" = source_file) %>%
  mutate(match_status_acoustic = "MISMATCH")

# Combine everything
df_final_merged_interval7 <- bind_rows(matches_survey_date_by_lake_interval7, unmatched_trawl_interval7, unmatched_acoustic_interval7)

# Remove columns that are not needed
df_final_merged_interval7 <- df_final_merged_interval7 %>%
  unite(col = "source_file", source_file_trawl, source_file_acoustic, sep = ", ") %>%
  arrange(ats_year, lake_name) %>%
  select(ats_year, lake_code, lake_name, survey_date_trawl, interval_start, interval_end,
         survey_date_acoustic, source_file, match_status_acoustic, overlap_status_acoustic, count_acoustic_survey, 
         -id_trawl, -id_acoustic, -date_diff)

# Remove NAs that were added to the rows
replacement_pattern <- c("NA, " = "", ", NA" = "")
df_final_merged_interval7$source_file <- str_replace_all(df_final_merged_interval7$source_file, replacement_pattern)

# Save final table in csv
write.csv(df_final_merged_interval7, paste0(final_directory, "/", "FINAL_7days_match_trawl_and_acoustic.csv"), row.names = FALSE)

###
##############        Checking inventories match / merge.    ##################
###

# Load inventory file
temporary_inventory_data <-read.csv(paste0(data_directory,"Combined_inventories.csv"))

### adding ats_year and trawl_month
temporary_inventory_data <- temporary_inventory_data %>%
  mutate(trawl_date = ymd(Date),
         trawl_month = month(Date),
         ats_year = year(Date) - if_else(trawl_month < 4, 1L, 0L))

# Join the tables to standardize lake names
temporary_inventory_data <- temporary_inventory_data %>%
  rename("lake_code" = Syscode) %>%
  left_join(lake_name, by = "lake_code") %>%
  arrange(ats_year, lake_name, trawl_date) %>%
  mutate(source_file = "Inventory") %>%
  select(ats_year, lake_code, lake_name, trawl_date, Date, System, Year, Trawl., Depth, 
         source_file, -trawl_month, -lake_longitude, -lake_latitude)

# create a data set with the info I want to check
temporary_inventory_data$lake_name <-gsub(" ", "_", temporary_inventory_data$lake_name)
unique_inventory_data <- tibble(unique = unique(paste(temporary_inventory_data$lake_code, temporary_inventory_data$lake_name, 
                                                      temporary_inventory_data$trawl_date, temporary_inventory_data$ats_year,
                                                      temporary_inventory_data$source_file))) 

unique_inventory_data <- unique_inventory_data %>%
  separate(col = unique, into = c("lake_code", "lake_name", "survey_date",
                                  "ats_year", "source_file"), sep = " ")

######### Create a match/merge table based on the intervals with the inventory

# Set the class of the date
unique_inventory_data$survey_date <- as.Date(unique_inventory_data$survey_date)

# Add unique IDs
unique_inventory_data <- unique_inventory_data %>%
  mutate(id_inventory = row_number())

# Include row number in the merged dataset so we can recover the rows that were not a match between inventory and trawl/acoustic
df_final_merged_inventory <- df_final_merged %>%
  mutate(id_general = row_number())

matches_all <- df_final_merged_inventory %>%
  full_join(unique_inventory_data, by = c("lake_name", "ats_year", "lake_code"), suffix = c("", "_inventory")) %>%
  mutate(date_diff_inventory = ifelse(!is.na(source_file_inventory), abs(as.numeric(survey_date_trawl - survey_date)), NA_real_)) %>%
  filter(date_diff_inventory <= 2) %>%
  mutate(match_status_inventory = ifelse(date_diff_inventory <= 2, "MATCH", "MISMATCH"))

# Unmatched id_inventory rows
unmatched_inventory <- unique_inventory_data %>%
  filter(!id_inventory %in% matches_all$id_inventory) %>%
  rename("source_file_inventory" = source_file) %>%
  mutate(match_status_inventory = "MISMATCH")

# Unmatched general rows from acoustic and trawl
unmatched_general <- df_final_merged_inventory %>%
  filter(!id_general %in% matches_all$id_general) %>%
  mutate(match_status_inventory = "MISMATCH")

# Combine everything
df_final_merged_inventory <- bind_rows(matches_all, unmatched_inventory, unmatched_general)

df_final_merged_inventory <- df_final_merged_inventory %>%
  rename("survey_date_inventory" = survey_date) %>%
  arrange(ats_year, lake_name, survey_date_trawl) %>%
  select(ats_year, lake_code, lake_name, survey_date_trawl, interval_start, interval_end, survey_date_acoustic, 
         survey_date_inventory, everything())

# Eliminate columns that are not needed
df_final_merged_inventory <- df_final_merged_inventory %>%
  unite(col = "source_file", source_file, source_file_inventory, sep = ", ") %>%
  mutate(final_trawl_match = case_when(match_status_acoustic == "MATCH" & match_status_inventory == "MATCH" ~ "Match BOTH acoustic and inventory",
                                       match_status_acoustic == "MATCH" & match_status_inventory == "MISMATCH" ~ "Match ACOUSTIC",
                                       match_status_acoustic == "MISMATCH" & match_status_inventory == "MATCH" ~ "Match INVENTORY",
                                 TRUE ~ "MISMATCH")) %>%
  select(-date_diff_inventory, -id_inventory, -count_acoustic_survey)

# Remove NAs that were added to the rows
replacement_pattern <- c("NA, NA, " = "", 	
                         "NA, " = "",
                         ", NA" = "")
df_final_merged_inventory$source_file <- str_replace_all(df_final_merged_inventory$source_file, replacement_pattern)

# Save final table in csv
write.csv(df_final_merged_inventory, paste0(final_directory, "/", "FINAL_2days_match_trawl_acoustic_inventory.csv"), row.names = FALSE)

###
############## Check how many columns match including transect and trawl number ##################
###

# create a data set with the info I want to check
unique_Trawl_data_transect <- tibble(unique = unique(paste(Trawl_data_date$lake_code, Trawl_data_date$lake_name, Trawl_data_date$survey_date, 
                                                           Trawl_data_date$interval_start, Trawl_data_date$interval_end, Trawl_data_date$ats_year, 
                                                           Trawl_data_date$source_file,Trawl_data_date$trawl_number))) 

unique_acoustic_data_transect <- tibble(unique = unique(paste(Acoustic_data_date$lake_code, Acoustic_data_date$lake_name, Acoustic_data_date$survey_date,
                                                              Acoustic_data_date$ats_year, 
                                                              Acoustic_data_date$source_file, Acoustic_data_date$transect)))

unique_Trawl_data_transect <- unique_Trawl_data_transect %>%
  separate(col = unique, into = c("lake_code", "lake_name", "survey_date", "interval_start","interval_end", 
                                  "ats_year", "source_file", "trawl_number"), sep = " ")

unique_acoustic_data_transect <- unique_acoustic_data_transect %>%
  separate(col = unique, into = c("lake_code","lake_name", "survey_date", "ats_year", "source_file", "transect"), sep = " ")

###
######### Create a match/merge table based on two-day intervals including the transect
###

unique_Trawl_data_transect$survey_date <- as.Date(unique_Trawl_data_transect$survey_date)
unique_acoustic_data_transect$survey_date <- as.Date(unique_acoustic_data_transect$survey_date)

# Add unique IDs
unique_Trawl_data_transect <- unique_Trawl_data_transect %>%
  mutate(id_df1 = row_number())
unique_acoustic_data_transect <- unique_acoustic_data_transect %>%
  mutate(id_df2 = row_number())

# Find valid matches
matches_survey_date_by_lake_transect <- unique_Trawl_data_transect %>%
  inner_join(unique_acoustic_data, by = c("lake_name", "ats_year", "lake_code"), suffix = c("_trawl", "_acoustic")) %>%
  mutate(date_diff = abs(as.numeric(survey_date_trawl - survey_date_acoustic))) %>%
  filter(date_diff <= 2) %>% 
  mutate(match_status = "MATCH")

# Unmatched df1 rows
unmatched_df1_transect <- unique_Trawl_data_transect %>%
  filter(!id_df1 %in% matches_survey_date_by_lake$id_df1) %>%
  rename("survey_date_trawl" = survey_date) %>%
  rename("source_file_trawl" = source_file) %>%
  mutate(match_status = "MISMATCH")

# Unmatched df2 rows
unmatched_df2_transect <- unique_acoustic_data_transect %>%
  filter(!id_df2 %in% matches_survey_date_by_lake$id_df2) %>%
  rename("survey_date_acoustic" = survey_date) %>%
  rename("source_file_acoustic" = source_file) %>%
  mutate(match_status = "MISMATCH")

# Combine everything
df_final_merged_transect <- bind_rows(matches_survey_date_by_lake_transect, unmatched_df1_transect, unmatched_df2_transect)

# Remove columns that are not needed
df_final_merged_transect <- df_final_merged_transect %>%
  unite(col = "source_file", source_file_trawl, source_file_acoustic, sep = ", ") %>%
  select(-id_df1, -id_df2, -date_diff)

# Remove NAs that were added to the rows
replacement_pattern <- c("NA, " = "", ", NA" = "")
df_final_merged_transect$source_file <- str_replace_all(df_final_merged_transect$source_file, replacement_pattern)

# Save final table in csv
write.csv(df_final_merged_transect, paste0(final_directory, "/", "unique_rows_match_with_transect.csv"), row.names = FALSE)
