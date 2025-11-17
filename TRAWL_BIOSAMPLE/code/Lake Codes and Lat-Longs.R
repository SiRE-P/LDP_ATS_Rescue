## --------------------------------------------------------------------------
## Lake Codes and Lat-Longs.R
##
## Purpose: Read SAS SYSFMT codes from text file and output lookup table of 
##          lake names and lat-longs for each LEP lake code.
## Author:  H Stiff
## Date:    25.11.17
## Notes:   
##          
## --------------------------------------------------------------------------

# Load required packages
library(stringr)
library(dplyr)

# Read the file
folder   <- "C:/DFO-MPO/OneDrive/OneDrive - DFO-MPO/PROJECTS/LDP - Living_Data_Project/ATS Rescue/Data/1_In_Process/0_Lookup_Tables"
sas_text <- readLines(paste(folder, "/lake_codes_and_latlongs.txt", sep=""))

# --- Extract SYSFMT section ---
sysfmt_start <- grep("VALUE SYSFMT", sas_text)
latlong_start <- grep("value latlong", sas_text)
sysfmt_lines <- sas_text[(sysfmt_start+1):(latlong_start-1)]

# Get all code-name pairs using regex
sysfmt_pairs <- str_extract_all(sysfmt_lines, "\\d+\\s*=\\s*'[^']+'")
sysfmt_pairs <- unlist(sysfmt_pairs)

sysfmt_df <- data.frame(
  lake_code = as.integer(str_extract(sysfmt_pairs, "^\\d+")),
  lake_name = str_extract(sysfmt_pairs, "'([^']+)'") %>%
    str_replace_all("'", "") %>%
    str_replace_all("\\bLk\\b", "Lake"))  # Replace 'Lk' with 'Lake'

# --- Extract latlong section ---
latlong_end <- grep("END of FILE", sas_text)
latlong_lines <- sas_text[(latlong_start+1):(latlong_end-1)]

latlong_pairs <- str_extract_all(latlong_lines, "\\d+\\s*=\\s*'[^']+'")
latlong_pairs <- unlist(latlong_pairs)

latlong_df <- data.frame(
  lake_code = as.integer(str_extract(latlong_pairs, "^\\d+")),
  latlong = str_extract(latlong_pairs, "'([^']+)'") %>% str_replace_all("'", ""))

# Split latlong into latitude and longitude
latlong_df <- latlong_df %>%
  mutate(
    lake_latitude = str_extract(latlong, "^[^X]+") %>% str_trim(),
    lake_longitude = str_extract(latlong, "(?<=X).*") %>% str_trim()) %>%
  select(-latlong)

# Merge both datasets
final_df <- sysfmt_df %>%
# inner_join(latlong_df, by = "lake_code") %>%
  left_join(latlong_df, by = "lake_code") %>%
  arrange(lake_name)

# Save to CSV
write.csv(final_df, file.path("./ACOUSTIC_TARGETS/data", "lake_codes.csv"), row.names = FALSE)
write.csv(final_df, file.path("./TRAWL_BIOSAMPLE/data",  "lake_codes.csv"), row.names = FALSE)