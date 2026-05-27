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
  left_join(lake_name, by = "lake_code")

#### Rename headers of Acoustic_data and Trawl_data columns to match them
Trawl_data <- Trawl_data %>% 
  rename("survey_date" = trawl_date)

# How many dates the lake was surveyed
aggregate(survey_date ~ lake_code, data = Trawl_data, FUN = function(x) length(unique(x))) # n = 66
aggregate(survey_date ~ lake_code, data = Acoustic_data, FUN = function(x) length(unique(x))) # n = 82

# summarize each table
Trawl_data %>%
  group_by(survey_date, lake_name, ats_year) %>%
  summarise(count_trawl = n()) %>%
  arrange(survey_date, lake_name) -> summary_table_trawl

Acoustic_data %>%
  group_by(survey_date, lake_name, ats_year) %>%
  summarise(count_acoustic = n()) %>%
  arrange(survey_date, lake_name) -> summary_table_acoustic

# combine and compare results
summary_table_trawl_acoustic <-full_join(summary_table_trawl, summary_table_acoustic, 
                                   by=c("survey_date", "lake_name", "ats_year"))

# Save final table in csv
write.csv(summary_table_trawl_acoustic, paste0(final_directory, "/", "record_count_trawl_acoustic_by_lake_and_date.csv"), row.names = FALSE)

# How many dates the lake was surveyed and number of trawls
aggregate(cbind(survey_date, ats_year) ~ lake_code, data = Acoustic_data, FUN = function(x) length(unique(x)))
aggregate(cbind(survey_date, ats_year) ~ lake_code, data = Trawl_data, FUN = function(x) length(unique(x)))

# Check rows that matches in each dataset 
matched_data_trawl <- semi_join(Trawl_data, Acoustic_data, by = c("survey_date", "lake_name", "ats_year"))
matched_data_acoustic <- semi_join(Acoustic_data, Trawl_data, by = c("survey_date", "lake_name", "ats_year"))

# Check rows that did not match in each dataset 
mismatched_data_trawl <- anti_join(Trawl_data, Acoustic_data, by = c("survey_date", "lake_name", "ats_year"))
mismatched_data_acoustic <- anti_join(Acoustic_data, Trawl_data, by = c("survey_date", "lake_name", "ats_year"))

# Check how many columns match allowing a fuzzy interval of 2 days
# guarantee date are in date, not character
Trawl_data_date <- Trawl_data %>%
  mutate(survey_date = as.Date(survey_date),
         interval_start = survey_date - 2,
         interval_end = survey_date + 2)

Acoustic_data_date <- Acoustic_data %>%
  mutate(survey_date = as.Date(survey_date))
         
# Check how many columns match
# create a data set with the info I want to check
Trawl_data_date$lake_name <-gsub(" ", "_", Trawl_data_date$lake_name)
unique_Trawl_data <- tibble(unique = unique(paste(Trawl_data_date$lake_code, Trawl_data_date$lake_name, Trawl_data_date$survey_date, 
                                       Trawl_data_date$interval_start, Trawl_data_date$interval_end, Trawl_data_date$ats_year))) 
Acoustic_data_date$lake_name <-gsub(" ", "_", Acoustic_data_date$lake_name)
unique_acoustic_data <- tibble(unique = unique(paste(Acoustic_data_date$lake_code, Acoustic_data_date$lake_name, Acoustic_data_date$survey_date,
                                                     Acoustic_data_date$ats_year)))

unique_Trawl_data <- unique_Trawl_data %>%
  separate(col = unique, into = c("lake_code", "lake_name", "survey_date", "interval_start","interval_end", 
                                  "ats_year"), sep = " ")

unique_acoustic_data <- unique_acoustic_data %>%
  separate(col = unique, into = c("lake_code","lake_name", "survey_date", "ats_year"), sep = " ")

# line up identical rows
columns <- c("lake_code", "lake_name", "survey_date", "ats_year")

# Check for leading/trailing whitespace
for (col in columns) {
  unique_acoustic_data[[col]] <- trimws(unique_acoustic_data[[col]])}

for (col in columns) {
  unique_Trawl_data[[col]] <- trimws(unique_Trawl_data[[col]])}

# Merge the database using the unique ID to combine
merged_df <- merge(unique_acoustic_data [, columns, drop = FALSE],
                   unique_Trawl_data [, c("interval_start", "interval_end", columns), drop = FALSE],
                   by = c("lake_name", "ats_year", "survey_date", "lake_code"),
                   all = TRUE, # Keep all
                   suffixes = c("_acoustic", "_trawl"))

######### Based on intervals

unique_Trawl_data$survey_date <- as.Date(unique_Trawl_data$survey_date)
unique_acoustic_data$survey_date <- as.Date(unique_acoustic_data$survey_date)

# Add unique IDs
unique_Trawl_data <- unique_Trawl_data %>%
  mutate(id_df1 = row_number())
unique_acoustic_data <- unique_acoustic_data %>%
  mutate(id_df2 = row_number())

# Step 1: Find valid matches
matches_survey_date_by_lake <- unique_Trawl_data %>%
  inner_join(unique_acoustic_data, by = c("lake_name", "ats_year", "lake_code"), suffix = c("_trawl", "_acoustic")) %>%
  mutate(date_diff = abs(as.numeric(survey_date_trawl - survey_date_acoustic))) %>%
  filter(date_diff <= 2) %>% 
  mutate(match_status = "MATCH")

# Step 2: Unmatched df1 rows
unmatched_df1 <- unique_Trawl_data %>%
  filter(!id_df1 %in% matches_survey_date_by_lake$id_df1) %>%
  rename("survey_date_trawl" = survey_date) %>%
  mutate(match_status = "MISMATCH")

# Step 3: Unmatched df2 rows
unmatched_df2 <- unique_acoustic_data %>%
  filter(!id_df2 %in% matches_survey_date_by_lake$id_df2) %>%
  rename("survey_date_acoustic" = survey_date) %>%
  mutate(match_status = "MISMATCH")

# Step 4: Combine everything
df_final_merged <- bind_rows(matches_survey_date_by_lake, unmatched_df1, unmatched_df2)

# Eliminate columns that are not needed
df_final_merged <- df_final_merged %>%
  select(-id_df1, -id_df2, -date_diff)

# Save final table in csv
write.csv(df_final_merged, paste0(final_directory, "/", "unique_rows_match.csv"), row.names = FALSE)








