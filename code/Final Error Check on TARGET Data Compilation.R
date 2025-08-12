## --------------------------------------------------------------------------
## Final Error Check on TARGET Data Compilation.R
##
## Author:  H Stiff
## Date:    250813
## Notes:   Import Yuliya's compiled TARGET data, summarize and look for errors.
##          
## --------------------------------------------------------------------------

# Load necessary library
library(dplyr)
library(lubridate)

# Define the file path
file_path <- "C:/DFO-MPO/OneDrive/OneDrive - DFO-MPO/PROJECTS/LDP - Living_Data_Project/ATS Rescue/4_Outputs/TARGET_1977_2007_combined_V8.csv"

# Read the CSV file
# target_data <- read.csv(file_path, stringsAsFactors = FALSE)

# Identify duplicates
target_data_duplicates <- target_data %>%
  mutate(line_number = row_number()) %>%
  group_by(lake_code, survey_date, transect, depth_code) %>%  # key variables for duplicates check
  filter(n() > 1) %>%
  ungroup()

# View the duplicate records with line numbers
View(target_data_duplicates)

# Export to CSV
write.csv(target_data_duplicates, "C:/DFO-MPO/OneDrive/OneDrive - DFO-MPO/PROJECTS/LDP - Living_Data_Project/ATS Rescue/3_Data/2_For_Review/combined_target_data/target_data_duplicates.csv", row.names = FALSE)

# Remove duplicates from the original dataset
target_data_no_dups <- target_data %>%
  distinct(lake_code, survey_date, transect, depth_code, .keep_all = TRUE)  # Same key variables for duplicates removal

# Check for missing data (should be zero for lake, year, date, transect, area, depth and targets variables...)
NA_missing_summary <- sapply(target_data_no_dups, function(x) sum(is.na(x)))
print(NA_missing_summary)

# Flag invalid dates
target_data_err_chk <- target_data_no_dups %>%
  
  mutate(
    parsed_date = ymd(survey_date, quiet = TRUE),
    invalid_date_flag = is.na(parsed_date),
    year_mismatch_flag = year(parsed_date) != survey_year,
    future_date_flag = parsed_date > Sys.Date(),
    line_number = row_number()) %>%
 
  filter(invalid_date_flag | year_mismatch_flag | future_date_flag)


# Summarize flagged records by lake, ats_year, and survey_date
target_err_summary <- target_data_err_chk %>%
  group_by(lake, ats_year, survey_date) %>%
  summarise(
    record_count = n(),
    invalid_dates = sum(invalid_date_flag),
    year_mismatches = sum(year_mismatch_flag),
    future_dates = sum(future_date_flag),
    .groups = "drop")

# Print all flagged records summary
print(target_err_summary)
