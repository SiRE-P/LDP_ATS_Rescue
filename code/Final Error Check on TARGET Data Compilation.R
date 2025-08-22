## --------------------------------------------------------------------------
## Final Error Check on TARGET Data Compilation.R
##
## Author:  H Stiff 
## Date:    250813
## Notes:   Import Yuliya's compiled TARGET data, review for duplicates and data issues.
##          
## --------------------------------------------------------------------------

# Load necessary library
library(dplyr)
library(lubridate)

# Import data ####
# Define the file paths
input_path  <- "C:/DFO-MPO/OneDrive/OneDrive - DFO-MPO/PROJECTS/LDP - Living_Data_Project/ATS Rescue/3_Data/2_For_Review/target_data/1_combined_target_data/"
output_path <- "C:/DFO-MPO/OneDrive/OneDrive - DFO-MPO/PROJECTS/LDP - Living_Data_Project/ATS Rescue/4_Outputs/"

file <- "TARGET_1977_2007_combined_V8.csv"
file <- "TARGET_1977_2007_combined_V9.csv"      # 220819
file <- "TARGET_1977_2007_combined_V9_YS.csv"   # 220820

# Read the CSV file
target_data <- read.csv(paste(input_path, file, sep=""), stringsAsFactors = FALSE)

# Add line_numbers to input data
target_data_line_nums <- target_data %>%
  mutate(line_number = row_number()) 

# Identify and remove exact duplicate records ####
target_data_exact_duplicates <- target_data_line_nums %>%
  filter(duplicated(select(., -line_number))) #  excluding line_number
View(target_data_exact_duplicates)
# Export to CSV
write.csv(target_data_exact_duplicates, paste(output_path, "target_data_exact_duplicates.csv", sep=""), row.names = FALSE)

# Remove exact duplicates from input data
target_data_exact_dups_removed <- target_data_line_nums %>%
  distinct(across(-line_number), .keep_all = TRUE) %>%
  arrange(lake_code, survey_date, transect, depth_code)

# Identify further duplicates on key fields #### 
target_data_keyfield_duplicates <- target_data_exact_dups_removed %>%
  group_by(lake_code, survey_date, transect, depth_code) %>%  # Key fields: lake_code, survey_date, transect, depth_code
  filter(n() > 1) %>%
  ungroup()
# View the duplicate records with line numbers
View(target_data_keyfield_duplicates)
# Export to CSV
write.csv(target_data_keyfield_duplicates, paste(output_path, "target_data_keyfield_duplicates.csv", sep=""), row.names = FALSE) 

# DO NOT REMOVE key field duplicates from the target_data_exact_dups_removed dataset
target_data_no_dups <- target_data_exact_dups_removed 
#  distinct(lake_code, survey_date, transect, depth_code, .keep_all = TRUE)  # Same key variables for duplicates removal

# View matching records in target_data_filtered
target_matching_keyfield_duplicates <- target_data_no_dups %>%
  semi_join(target_data_keyfield_duplicates, by = c("lake_code", "survey_date", "transect", "depth_code")) %>%
  arrange(lake_code, survey_date, transect, survey_comments, depth_code) 
View(target_matching_keyfield_duplicates)

# Categorical consistency checks ####
unique(paste(target_data_no_dups$lake, target_data_no_dups$lake_code), sep="")  # check for unique/valid lake x lake_code combinations
unique(paste(target_data_no_dups$sounder_type, target_data_no_dups$sounder_code), sep="")  # check for unique/valid sounder x sounder_code combinations

# Check for missing data #### 
# Should be zero missing for lake, year, date, transect, area, depth and targets variables...
NA_missing_summary <- sapply(target_data_no_dups, function(x) sum(is.na(x)))
print(NA_missing_summary)

# Check for invalid dates ####
target_date_err_chk <- target_data_no_dups %>%
  mutate(
    parsed_date = ymd(survey_date, quiet = TRUE),
    invalid_date_flag = is.na(parsed_date),                  # Flag invalid dates
    year_mismatch_flag = year(parsed_date) != survey_year,
    future_date_flag = parsed_date > Sys.Date(),   
    line_number = row_number()) %>%
 
  filter(invalid_date_flag | year_mismatch_flag | future_date_flag)

# Summarize flagged invalid date records by lake, ats_year, and survey_date
target_date_err_summary <- target_date_err_chk %>%
  group_by(lake, ats_year, survey_date) %>%
  summarise(
    record_count = n(),
    invalid_dates = sum(invalid_date_flag),
    year_mismatches = sum(year_mismatch_flag),
    future_dates = sum(future_date_flag),
    min_line_number = min(line_number),
    max_line_number = max(line_number),
    .groups = "drop")

# Print flagged dates summary, with approximate line-numbers in the data
print(target_date_err_summary)

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

# ATS Year classification ####
ATS_year_issues <- target_data_no_dups %>%
  mutate(
    survey_date_chk  = ymd(survey_date), 
    survey_year_chk  = year(survey_date),
    survey_month_chk = month(survey_date),
    ats_year_chk = if_else(survey_month_chk <= 3, survey_year_chk - 1, survey_year_chk), # BASED on April_YYYY to March_YYYY+1
    ats_year_error = ats_year_chk != ats_year) %>%
  filter(ats_year_error == TRUE) %>%
  dplyr::select(lake_code, lake, survey_date, survey_month, ats_year, ats_year_chk, ats_year_error) %>%
  distinct(., .keep_all = TRUE) 
View(ATS_year_issues)

# Check lake-specific data issues previously noted ####
megin_lake_issue <- target_data_no_dups %>% # should be a survey for March 20th, 1996
  filter(lake_code == 118) %>%
  dplyr::select(lake_code, lake, survey_date, survey_month, ats_year, source_file, data_issues) %>%
  distinct(., .keep_all = TRUE) 

muriel_lake_issue <- target_data_no_dups %>% # should be a survey for March 22, 1996
  filter(lake_code == 44) %>%
  dplyr::select(lake_code, lake, survey_date, survey_month, ats_year, source_file, data_issues) %>%
  distinct(., .keep_all = TRUE) 

tats_lake_issue <- target_data_no_dups %>% # should be a survey for AUG 1, 1992, with comment regarding being stored in 1995 datafile
  filter(lake_code == 66) %>%
  dplyr::select(lake_code, lake, survey_date, survey_month, ats_year, source_file, data_issues) %>%
  distinct(., .keep_all = TRUE) 

owikeno_A_lake_issue <- target_data_no_dups %>% # should be a survey for FEB 14 2007, but NO survey for FEB 15, 2007
  filter(lake_code == 228) %>%
  dplyr::select(lake_code, lake, survey_date, survey_month, ats_year, source_file, data_issues) %>%
  distinct(., .keep_all = TRUE) 

owikeno_B_lake_issue <- target_data_no_dups %>% # should be a survey for FEB 14 2007, but NO survey for FEB 15, 2007 or FEB 04, 2004
  filter(lake_code == 229) %>%
  dplyr::select(lake_code, lake, survey_date, survey_month, ats_year, source_file, data_issues) %>%
  distinct(., .keep_all = TRUE) 

# End of Program ####
