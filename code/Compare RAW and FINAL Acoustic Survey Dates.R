## --------------------------------------------------------------------------
## Compare RAW and FINAL Acoustic Survey Dates.R
##
## Purpose: Match-merge RAW and FINAL acoustic survey metadata 
##          (i.e., by lake and survey_date) from LDP program
##          Acoustic Target Data Cleanup.R to ensure nothing lost in process.
##
## Author:  H Stiff
## Date:    Sep 2025
## Notes:   
##          
## --------------------------------------------------------------------------

library(dplyr)

# Step 1: Select relevant columns and get distinct combinations
raw_target_data_reduced <- all_target_data %>%   # <- this is the RAW target data
  select(lake_code, lake, survey_date, acoustic_survey_notes, source_file) %>%
  distinct()

final_target_data_reduced <- merged_data_final_chk %>%  # <- this is the CLEANED target data
  select(lake_code, lake, survey_date, acoustic_survey_notes, source_file) %>%
  distinct()

# Step 2: Add source indicators
raw_target_data_reduced <- raw_target_data_reduced %>%
  mutate(source = "RAW_target_data")

final_target_data_reduced <- final_target_data_reduced %>%
  mutate(source = "FINAL_target_data")

# Step 3: Full join to combine both datasets
combined1 <- full_join(raw_target_data_reduced, final_target_data_reduced,
                      by = c("lake_code", "survey_date"),
                      suffix = c("_all", "_merged"))
# Warning message:
#   In full_join(raw_target_data_reduced, final_target_data_reduced,  :
#                  Detected an unexpected many-to-many relationship between `x` and `y`.
#  ℹ Row 555 of `x` matches multiple rows in `y`.
#  ℹ Row 551 of `y` matches multiple rows in `x`.
#  ℹ If a many-to-many relationship is expected, set `relationship = "many-to-many"` to silence this warning.

# Step 4: Determine final source label
combined2 <- combined1 %>%
  mutate(source = case_when(
    !is.na(source_all) & !is.na(source_merged) ~ "both",
    !is.na(source_all) ~ "RAW_target_data_only",
    !is.na(source_merged) ~ "FINAL_target_data_only"
  )) %>%
  # Optional: clean up intermediate source columns
  select(lake_code, survey_date, lake_all, lake_merged,
         acoustic_survey_notes_all, acoustic_survey_notes_merged,
         source_file_all, source_file_merged, source)

# Optional: rename columns for clarity
combined_raw_and_final <- combined2 %>%
  mutate(ats_year = assign_ats_year(survey_date)) %>%
  rename(
    lake_all_target = lake_all,
    lake_merged_data = lake_merged,
    notes_all_target = acoustic_survey_notes_all,
    notes_merged_data = acoustic_survey_notes_merged,
    file_all_target = source_file_all,
    file_merged_data = source_file_merged) %>%
  select(source, ats_year, lake_code, survey_date, everything()) %>%
  arrange(source, ats_year, lake_code, survey_date)

write_csv(combined_raw_and_final, paste("./output/target_merge_RAW_FINAL_", date_stamp, ".csv", sep=""))
