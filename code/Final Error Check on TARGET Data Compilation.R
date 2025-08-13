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
target_data <- read.csv(file_path, stringsAsFactors = FALSE)

# Add line_numbers to input data
target_data_line_nums <- target_data %>%
  mutate(line_number = row_number()) 

# Identify and display exact duplicate rows (excluding line_number)
target_exact_duplicates <- target_data_line_nums %>%
  filter(duplicated(select(., -line_number)))

# View the duplicate records
# View(target_exact_duplicates)

# Export to CSV
write.csv(target_exact_duplicates, "C:/DFO-MPO/OneDrive/OneDrive - DFO-MPO/PROJECTS/LDP - Living_Data_Project/ATS Rescue/3_Data/2_For_Review/combined_target_data/target_exact_duplicates.csv", row.names = FALSE)

# Remove exact duplicates from input data
target_data_exact_dups_removed <- target_data_line_nums %>%
  distinct(across(-line_number), .keep_all = TRUE) %>%
  arrange(lake_code, survey_date, transect, depth_code)

# Identify further duplicates based on key fields: lake_code, survey_date, transect, depth_code
target_keyfield_duplicates <- target_data_exact_dups_removed %>%
  group_by(lake_code, survey_date, transect, depth_code, targets) %>%  # key variables for duplicates check
  filter(n() > 1) %>%
  ungroup()

# View the duplicate records with line numbers
# View(target_keyfield_duplicates)

# Export to CSV
write.csv(target_keyfield_duplicates, "C:/DFO-MPO/OneDrive/OneDrive - DFO-MPO/PROJECTS/LDP - Living_Data_Project/ATS Rescue/3_Data/2_For_Review/combined_target_data/target_keyfield_duplicates.csv", row.names = FALSE)

# Remove key field duplicates from the target_data_exact_dups_removed dataset
target_data_no_dups <- target_data_exact_dups_removed %>%
  distinct(lake_code, survey_date, transect, depth_code, .keep_all = TRUE)  # Same key variables for duplicates removal

# View matching records in target_data_filtered
target_matching_keyfield_duplicates <- target_data_no_dups %>%
  semi_join(target_keyfield_duplicates, by = c("lake_code", "survey_date", "transect", "depth_code")) %>%
  arrange(lake_code, survey_date, transect, depth_code)

View(target_matching_keyfield_duplicates)

# Categorical consistency checks ####
unique(paste(target_data$lake, target_data$lake_code), sep="")  # check for unique/valid lake x lake_code combinations

# Check for missing data #### should be zero missing for lake, year, date, transect, area, depth and targets variables...
NA_missing_summary <- sapply(target_data_no_dups, function(x) sum(is.na(x)))
print(NA_missing_summary)

# Check for invalid dates ####
# Flag invalid dates
target_data_err_chk <- target_data_no_dups %>%
  
  mutate(
    parsed_date = ymd(survey_date, quiet = TRUE),
    invalid_date_flag = is.na(parsed_date),
    year_mismatch_flag = year(parsed_date) != survey_year,
    future_date_flag = parsed_date > Sys.Date(),
    line_number = row_number()) %>%
 
  filter(invalid_date_flag | year_mismatch_flag | future_date_flag)

# Summarize flagged invalid date records by lake, ats_year, and survey_date
target_err_summary <- target_data_err_chk %>%
  group_by(lake, ats_year, survey_date) %>%
  summarise(
    record_count = n(),
    invalid_dates = sum(invalid_date_flag),
    year_mismatches = sum(year_mismatch_flag),
    future_dates = sum(future_date_flag),
    min_line_number = min(line_number),
    max_line_number = max(line_number),
    .groups = "drop")

# Print flagged dates summary
print(target_err_summary)

# Range checks ####
range_issues <- target_data_no_dups %>%
  filter(depth_min_m > depth_max_m | targets < 0 | transect_length_m < 0 | area_ha < 0 | sounder_gain <= 0)
View(range_issues)

# Logical Relationships ####
logical_relation_issues <- target_data_no_dups %>%
  
  mutate(
    prop_sum = prop_sockeye + prop_stickleback,
    prop_sum_flag = prop_sum < 0.99 | prop_sum > 1.01) %>% # check if the sum of sox and stix proportions == 1 (with 0.01 fuzz factor)
  filter(!is.na(prop_sum_flag) & prop_sum_flag)
View(logical_relation_issues)


