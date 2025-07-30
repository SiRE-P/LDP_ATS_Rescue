library(dplyr)
library(janitor)
library(readr)
library(RODBC)

# Import trawl data from Access database, subset for ATS years 1984-1986 and match-merge with trawl data from (cleaned up) Apple datasets

# Connect to your Access database:
setwd("C:\\Users\\StiffH\\Documents\\FISHERIES\\SALMON INDEX STOCKS\\Trawl\\TrawlData")
getwd()

conn <- odbcConnectAccess2007("Trawl Database.accdb") # ("Trawl Database 77-98 (No AGEs).accdb")

# Fetch a table:
Access_trawl_samples <- sqlFetch(conn, "TrawlSamples") 

# Close connection:
odbcClose(conn)

# filter for the few years where we cross-check with Apple data... 
Access_trawl_data <- Access_trawl_samples %>%
  janitor::clean_names() %>%
  filter(trawl_year %in% c(1984, 1985, 1986)) %>%
  mutate(lake_code = system) %>%
  mutate(ats_year = trawl_year) %>%
  mutate(trawl_date = date) %>%
  mutate(trawl_number = trawl) %>%
  mutate(species_code = species) %>%
  mutate(fish_id = fish) %>%
  dplyr::select(-system, -trawl_year, -date, -trawl, -species, -fish)

# Save as CSV
write.csv(Access_trawl_data,"Access_trawl_samples_848586.csv")

# APPLE DATA ###
# Input trawl data from Apple files as cleaned up by Yuliya S
setwd("C:\\DFO-MPO\\OneDrive\\OneDrive - DFO-MPO\\PROJECTS\\LDP - Living_Data_Project\\ATS Rescue\\3_Data\\2_For_Review\\trawl_data")
getwd()

# Read the CSV files for the APPLE data
Apple_trawl_data_84 <- read_csv("TRAWL84_V2.csv") %>%
  mutate(depth_m = as.numeric(depth_m), 
         duration_min = as.numeric(duration_min),
         processor = as.character(processor),
         scale_book_number = as.character(scale_book_number))

Apple_trawl_data_85 <- read_csv("TRAWL85_V2.csv") %>%
  mutate(depth_m = as.numeric(depth_m), 
         duration_min = as.numeric(duration_min),
         processor = as.character(processor),
         scale_book_number = as.character(scale_book_number))

Apple_trawl_data_86 <- read_csv("TRAWL86_V2.csv") %>%
  mutate(depth_m = as.numeric(depth_m), 
         duration_min = as.numeric(duration_min),
         processor = as.character(processor),
         scale_book_number = as.character(scale_book_number))

# Combine the three data frames
Apple_trawl_data <- bind_rows(Apple_trawl_data_84, Apple_trawl_data_85, Apple_trawl_data_86)

# Save as CSV
write.csv(Apple_trawl_data,"Apple_trawl_samples_848586.csv")

# Check for exact duplicates across all columns
# Convert all columns to character to avoid type coercion issues
apple_clean <- Apple_trawl_data %>%
  mutate(across(everything(), as.character))

# Now check for exact duplicates
duplicate_rows <- apple_clean %>%
  filter(duplicated(.))

# Count and inspect
cat("Apple Data: Number of exact duplicates (after coercing to character):", nrow(duplicate_rows), "\n")
print(duplicate_rows)

#---
# Convert all Access columns to character to avoid type coercion issues
access_clean <- Access_trawl_data %>%
  mutate(across(everything(), as.character))

# Now check for exact duplicates
duplicate_rows <- access_clean %>%
  filter(duplicated(.))

# Count and inspect
cat("Access Data: Number of exact duplicates (after coercing to character):", nrow(duplicate_rows), "\n")
print(duplicate_rows)

# MERGE APPLE AND ACCESS DATA and ...

# Define the join keys
join_keys <- c("lake_code", "ats_year", "trawl_date", "trawl_number", "species_code", "fish_id")

# Step 1: Identify exact matches
exact_matches <- inner_join(Access_trawl_data, Apple_trawl_data, by = join_keys)

# Step 2: Identify non-matches (rows in either dataset but not both)
non_matches <- full_join(Access_trawl_data, Apple_trawl_data, by = join_keys) %>%
  anti_join(exact_matches, by = join_keys)

# Step 3: Identify many-to-many matches
# Count duplicates in each dataset
access_dupes <- Access_trawl_data %>%
  group_by(across(all_of(join_keys))) %>%
  filter(n() > 1)

apple_dupes <- Apple_trawl_data %>%
  group_by(across(all_of(join_keys))) %>%
  filter(n() > 1)

# Join duplicate keys from both datasets to find many-to-many matches
many_to_many_matches <- inner_join(access_dupes, apple_dupes, by = join_keys)

# Optional: write to CSV
# write.csv(exact_matches, "exact_matches.csv", row.names = FALSE)
# write.csv(non_matches, "non_matches.csv", row.names = FALSE)
# write.csv(many_to_many_matches, "many_to_many_matches.csv", row.names = FALSE)



