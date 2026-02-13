# Check_Errors_Out_CSVs.R                                                   ####
##
## Author:  H Stiff
## Date:    11-Feb-2026
## Notes:   This script reads all CSV files from the "03_errors_out" subfolder,
##          and filters the data frames to the relevant columns, in a standard
##          order of columns for quicker review of data issues.

# SETUP libraries and functions                                             ####
# Load packages (install if needed)
suppressPackageStartupMessages({
  library(dplyr)   # data manipulation
  library(readr)   # fast & friendly CSV import
  library(purrr)   # functional mapping
  library(stringr) # text-string helpers
  library(fs)      # robust path handling (optional but nice)
})

# Path to subfolder "03_errors_out" under current working directory
errors_dir <- file.path(getwd(), "TRAWL_BIOSAMPLE/03_errors_out")
finals_dir <- file.path(getwd(), "TRAWL_BIOSAMPLE/04_final_output")
final_data <- readr::read_csv(file.path(finals_dir, "Trawl_data_FINAL_1977-1999.csv"), show_col_types = FALSE)

# Safety check
if (!dir.exists(errors_dir)) {
  stop(sprintf("Directory does not exist: %s", errors_dir))
}

# Find CSV files (non-recursive; set recursive = TRUE if you have nested folders)
csv_files <- list.files(errors_dir, pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)

if (length(csv_files) == 0) {
  stop(sprintf("No CSV files found in: %s", errors_dir))
}

# Create names from file basenames without extension; make syntactically valid names
obj_names <- tools::file_path_sans_ext(basename(csv_files)) |>
  make.names(unique = TRUE)

# READ all CSVs into a named list                                           ####
#   Adjust readr::read_csv() arguments as needed (e.g., locale, col_types)
errors_dfs <- map(csv_files, ~ readr::read_csv(.x, show_col_types = FALSE)) |>
  set_names(obj_names)

length(errors_dfs)                       # count of data frames loaded
str(errors_dfs, max.level = 1)           # Peek at what was loaded  # Access like: errors_dfs$SomeFileName
list2env(errors_dfs, envir = .GlobalEnv) # split into separate dataframes

# REVIEW duplicated_df_trawl_chk                                            ####
# Commented out as now all duplicates are kept but flagged in data_issues column...

# Define the relevant columns to keep for resolving data issue
# duplicated_df_trawl_chk <- duplicated_df_trawl %>%
#   arrange(lake_name, ats_year, trawl_date, trawl_number, fish_unique_ID) %>%
#   select(lake_name, ats_year, trawl_date, trawl_number, depth_m, source_files, trawl_unique_ID, fish_unique_ID, 
#          merging_update_type, species_code, fish_description, species_code_comment, 
#          fish_length_mm, fish_weight_g, standardized_weight_g, preservative_code, everything())
# # for Stickleback duplicates, there may be some added information in the species_code_comment column
# stickles_chk <- duplicated_df_trawl_chk %>%
#   filter(species_code == 2) %>%
#   arrange(lake_name, ats_year, trawl_date, trawl_number, fish_unique_ID)
# 
# species_summary <- duplicated_df_trawl_chk %>%
#   group_by(lake_name, species_code, species_code_comment) %>%
#   summarize(
#     n_fish = n(),
#     mean_length_mm = mean(fish_length_mm, na.rm = TRUE),
#     sd_length_mm   = sd(fish_length_mm, na.rm = TRUE),
#     min_length_mm  = min(fish_length_mm, na.rm = TRUE),
#     max_length_mm  = max(fish_length_mm, na.rm = TRUE),
#     
#     mean_weight_g = mean(fish_weight_g, na.rm = TRUE),
#     sd_weight_g   = sd(fish_weight_g, na.rm = TRUE),
#     min_weight_g  = min(fish_weight_g, na.rm = TRUE),
#     max_weight_g  = max(fish_weight_g, na.rm = TRUE),
#     
#     .groups = "drop")
# 
# species_summary


# REVIEW no_species_record_rows_chk                                         ####
#   Define the relevant columns to keep for resolving data issue
no_species_record_rows_chk <- no_species_record_rows %>%
  select(
    lake_name,
    ats_year,
    trawl_date,
    trawl_number,
    depth_m,
    source_files,
    fish_unique_ID,
    species_info_code  ,
    species_code_comment ,
    fish_description  ,
    fish_id        ,
    fish_length_mm      ,
    fish_weight_g    ,
    age,
    aging_technique_name,
    comment
  ) %>%
  arrange(lake_name, ats_year, trawl_date, trawl_number, fish_unique_ID)

# Compare with final_dataframe to see which rows are present or missing as these should not be omitted
exists_rows <- no_species_record_rows_chk %>%
  semi_join(final_data %>% select(fish_unique_ID), by = "fish_unique_ID") %>%
  left_join(final_data %>% select(fish_unique_ID, data_issues),  # shows data_issues field updated correctly
            by = "fish_unique_ID") %>%
  arrange(lake_name, ats_year, trawl_date, trawl_number, fish_unique_ID)
exists_rows

missing_rows <- no_species_record_rows_chk %>% # shows no rows that are missing from final_data, which is good as these should not be omitted
  anti_join(final_data %>% select(fish_unique_ID), by = "fish_unique_ID") %>%
  arrange(lake_name, ats_year, trawl_date, trawl_number, fish_unique_ID)
missing_rows

# REVIEW fish_length_weight_errors                                          ####
#   Define the relevant columns to keep for resolving data issue
fish_length_weight_errors_chk <- fish_length_weight_errors %>%
  select(fish_unique_ID,
    lake_name,
    lake_code,
    ats_year,
    trawl_date,
    species_code  ,
    fish_description  ,
    fish_length_mm      ,
    fish_weight_g    ,
    species_code_comment ,
    age, 
    trawl_number, fish_id,
    depth_m,
    source_files
  ) %>%
  arrange(lake_name, ats_year, trawl_date, trawl_number, fish_unique_ID)

# filter for specific lake and species for each record in fish_length_weight_errors to get size stats & distribution
lake_species_select <- final_data %>% filter(lake_code == 107 & species_code == 2)  # Cheewhat stickleback
lake_species_select <- final_data %>% filter(lake_code == 29  & species_code == 2)  # Devon stickleback
lake_species_select <- final_data %>% filter(lake_code == 214 & species_code == 2)  # Dragon stickleback
lake_species_select <- final_data %>% filter(lake_code == 3   & species_code == 2)  # Henderson stickleback
lake_species_select <- final_data %>% filter(lake_code == 41  & species_code == 2)  # Kennedy Main stickleback
lake_species_select <- final_data %>% filter(lake_code == 41  & species_code == 1)  # Kennedy Main Sockeye
lake_species_select <- final_data %>% filter(lake_code == 8   & species_code == 1 & fish_weight_g < 10)  # Long Lake Sockeye
lake_species_select <- final_data %>% filter(lake_code == 8   & species_code == 2)  # Long Lake stickleback
lake_species_select <- final_data %>% filter(lake_code == 802 & species_code == 2)  # Long Lake stickleback
lake_species_select <- final_data %>% filter(lake_code == 44  & species_code == 1 & fish_weight_g < 10)  # Muriel Sockeye
lake_species_select <- final_data %>% filter(lake_code == 18  & species_code == 1)  # Owikeno Sockeye
lake_species_select <- final_data %>% filter(lake_code == 18  & species_code == 2 & fish_weight_g < 10)  # Owikeno Stickleback
lake_species_select <- final_data %>% filter(lake_code == 66  & species_code == 1)  # Tatsamenie Sockeye
lake_species_select <- final_data %>% filter(lake_code == 23  & species_code == 1)  # Vernon Sockeye
lake_species_select <- final_data %>% filter(lake_code == 22  & species_code == 1)  # Woss Sockeye

# get 5th, 50th, and 95th percentiles for length and weight vars for histogram output
stats <- lake_species_select %>% 
  group_by(lake_name, species_code, species_common_name, life_stage) %>%
  summarize(
    len_mm_p05 = as.numeric(quantile(fish_length_mm, probs = 0.05, na.rm = TRUE)),  
    len_mm_p50 = median(fish_length_mm, na.rm = TRUE),
    len_mm_p95 = as.numeric(quantile(fish_length_mm, probs = 0.95, na.rm = TRUE)),
    
    wt_mm_p05  = as.numeric(quantile(fish_weight_g, probs = 0.05, na.rm = TRUE)),
    wt_mm_p50  = median(fish_weight_g, na.rm = TRUE),
    wt_mm_p95  = as.numeric(quantile(fish_weight_g, probs = 0.95, na.rm = TRUE)),
    wt_mm_max  = as.numeric(quantile(fish_weight_g, probs = 1.00, na.rm = TRUE)),
    n_fish = n(),
    .groups = "drop")
stats

# Extract lake and species for dynamic titles
# lake_title    <- unique(lake_species_select$lake_name)
# species_title <- unique(lake_species_select$species_common_name)
# hist(lake_species_select$fish_length_mm, breaks = 30, main = "Length Distribution", xlab = "Fish Length (mm)")
# hist(lake_species_select$fish_weight_g,  breaks = 30, main = "Weight Distribution", xlab = "Fish Weight (g)")

# Function to plot stacked lake/species fish length/weight distributions histograms
plot_length_weight_hist <- function(df) {
  op <- par(mfrow = c(2, 1))  # Plot layout: 2 rows, 1 column
  
  # Extract lake and species for dynamic titles
  lake_title    <- unique(df$lake_name)
  species_title <- unique(df$species_common_name)
  
  # Length histogram
  hist(
    df$fish_length_mm,
    breaks = 30,
    main = paste0("Length Distribution - ", lake_title, " (Species:", species_title, ")"),
    xlab = "Fish Length (mm)")
  
  # Weight histogram
  hist(
    df$fish_weight_g,
    breaks = 30,
    main = paste0("Weight Distribution - ", lake_title, " (Species:", species_title, ")"),
    xlab = "Fish Weight (g)")
  
  # Reset plot layout to default
  par(op)
}

plot_length_weight_hist(lake_species_select)

# RESULTS
# change 1987-11-26_107_4_16_2_23_1.74_555  length to 55 mm from 555 and flag as likely typo
# change 1987-07-31_29_5_0_2_2_1.05_0.49    length to 49 mm from 0.49 and flag as likely typo
# change 1992-08-17_214_2_4_2_156_0.23_0.33 length to 33 mm from 0.33
# change 1993-02-25_3_3_55_2_1_96_45        weight to .95  from 95 g and flag as likely typo
# change 1985-12-11_41_4_15_2_16_0.34_3     length to 30 mm from 3, weight looks okay
# change 1987-02-25_41_1_7_27_2_0.19_0.3    length to 30 mm from .30
# change 1997-02-20_41_5_8_26_72_0.11_0.28  length to 28 mm from .28
# change 1989-06-03_41_15_10_7_78_30_0.26   length to 26 mm from .26
# change 1989-06-03_41_15_10_7_78_30_0.26   weight to .30  from 30
# change 1987-09-09_8_8_20_1_135_0.66_400   length to 40 mm from 400
# change 1993-07-24_8_4_16_7_46_60_39       weight to 6 from 60 g
# change 1987-09-09_8_8_20_1_146_0.64_399   length to 39 mm from 399
# change 1987-09-09_8_8_20_1_21_1.31_500    length to 50 mm from 500
# change 1987-09-09_8_9_7_2_81_0.84_444     length to 44 mm from 444
# change 1987-09-09_8_10_7_2_55_1.46_544    length to 54 mm from 544
# change 1987-09-09_8_13_20_1_42_1.033_455  length to 45 mm from 455
# change 1987-09-09_8_13_20_1_54_1.36_500   length to 50 mm from 500
# change 1995-08-30_8_7_7_2_57_999_4        length to 40 mm from 4
# change 1995-08-30_8_7_7_2_57_999_4        weight to NA from 999
# change 1990-06-22_802_1_6_2_48_0.46_0.38  length to 38 mm from .38
# change 1999-09-08_802_2_8_7_268_0_0       length and weight to NA
# change 1987-11-29_118_8_20_9_1_0_1.88     weight to NA instead of 0
# change 1989-02-22_44_3_30_1_3_0.73_4      length to 40 mm from 4
# change 1995-08-26_18_19_11_7_21_0.61_0.37 length to 37 mm from .37
# change 1995-08-26_18_11_0_7_137_46_35     weight to 4.6 mm from 46
# change 1995-08-26_18_13_7_7_93_39_53      length to 3.9 mm from 39
# change 1991-03-12_18_2_7_2_8_36_33        weight to 3.6 from 36
# change 1996-08-22_18_25_0_2_70_0.11_2.5   length to 25 mm from 2.5
# change 1996-08-22_18_25_0_2_71_0.16_2.7   length to 27 mm from 2.7
# change 1991-09-14_66_99_10_7_2_1.13_5     length to 50 mm from 5
# change 1992-08-12_23_4_31_7_36_90_45      weight to .90 from 90
# change 1992-08-12_22_1_24_7_133_97_46     weight to .97 mm from 97
# change 1992-08-12_22_1_24_7_126_66_41     weight to .66 from 66
# change 1992-08-12_22_2_28_7_134_66_42     weight to .66 from 66
# change 1992-08-12_22_2_28_7_133_105_46    weight to 1.05 from 105


# REVIEW start_time_errors_chk                                              ####
#   Define the relevant columns to keep for resolving data issue

# Columns to keep after reducing # of records with time issues using unique() on the meta-data fields alone
keep_cols <- c(
  "trawl_unique_ID",
  "lake_name",
  "lake_code",
  "ats_year",
  "trawl_date", "trawl_number", "depth_m",
  "start_time", "end_time", 
  "species_code",
  "fish_description",
  "source_files",
  "comment")

# 1) Build the working table and tally non-NA lengths per future-unique group
start_time_errors_chk <- start_time_errors %>%
  # add a per-group tally of non-NA lengths over the *keep_cols* grouping
  add_count(across(all_of(keep_cols)),
            wt = !is.na(fish_length_mm),
            name = "fish_lengths") %>%
  select(
    trawl_unique_ID, fish_unique_ID,
    lake_name, lake_code, ats_year,
    trawl_date, trawl_number, depth_m,
    start_time, end_time, duration_mi,
    species_code, fish_description, fish_lengths,
    fish_length_mm, fish_weight_g,
    fish_id, source_files, comment) %>%
  arrange(lake_name, ats_year, trawl_date, trawl_number, trawl_unique_ID, fish_unique_ID)

# 2) Now make the unique table; the tally of records (fish_lengths) associated with each trawl is preserved
start_time_errors_unique_chk <- start_time_errors_chk %>%
  select(
    trawl_unique_ID, 
    lake_name, lake_code, ats_year,
    trawl_date, trawl_number, depth_m,
    start_time, end_time,
    species_code, fish_description, fish_lengths,
    source_files, comment) %>%
  unique()

# Compare with final_dataframe to see which rows are present or missing as these should not be omitted
exists_rows <- start_time_errors_chk %>%
  semi_join(final_data %>% select(fish_unique_ID), by = "fish_unique_ID") %>%
  left_join(final_data %>% select(fish_unique_ID, data_issues),  # shows data_issues field updated correctly
            by = "fish_unique_ID") %>%
  arrange(lake_name, ats_year, trawl_date, trawl_number, fish_unique_ID)
exists_rows

missing_rows <- start_time_errors_chk %>% # shows no rows that are missing from final_data, which is good as these should not be omitted
  anti_join(final_data %>% select(fish_unique_ID), by = "fish_unique_ID") %>%
  arrange(lake_name, ats_year, trawl_date, trawl_number, fish_unique_ID)
missing_rows
# REVIEW end_time_errors_chk                                                ####
#   Define the relevant columns to keep for resolving data issue

# Columns to keep after reducing # of records with time issues using unique() on the meta-data fields alone
keep_cols <- c(
  "trawl_unique_ID",
  "lake_name",
  "lake_code",
  "ats_year",
  "trawl_date", "trawl_number", "depth_m",
  "start_time", "end_time", "duration_mi",
  "species_code",
  "fish_description",
  "source_files",
  "comment")

# 1) Build the working table and tally non-NA lengths per future-unique group
end_time_errors_chk <- end_time_errors %>%
  # add a per-group tally of non-NA lengths over the *keep_cols* grouping
  add_count(across(all_of(keep_cols)),
            wt = !is.na(fish_length_mm),
            name = "fish_lengths") %>%
  select(
    trawl_unique_ID, fish_unique_ID,
    lake_name, lake_code, ats_year,
    trawl_date, trawl_number, depth_m,
    start_time, end_time, duration_mi,
    species_code, fish_description, fish_lengths,
    fish_length_mm, fish_weight_g,
    fish_id, source_files, comment) %>%
  arrange(lake_name, ats_year, trawl_date, trawl_number, trawl_unique_ID, fish_unique_ID)

# 2) Now make the unique table; the tally of records (fish_lengths) associated with each trawl is preserved
end_time_errors_unique_chk <- end_time_errors_chk %>%
  select(
    trawl_unique_ID, 
    lake_name, lake_code, ats_year,
    trawl_date, trawl_number, depth_m,
    start_time, end_time, duration_mi,
    species_code, fish_description, fish_lengths,
    source_files, comment) %>%
  unique()  # Result: invalid end_times can all be set to NA

# Compare with final_dataframe to see which rows are present or missing as these should not be omitted
exists_rows <- start_time_errors_chk %>%
  semi_join(final_data %>% select(fish_unique_ID), by = "fish_unique_ID") %>%
  left_join(final_data %>% select(fish_unique_ID, data_issues),  # shows data_issues field updated correctly
            by = "fish_unique_ID") %>%
  arrange(lake_name, ats_year, trawl_date, trawl_number, fish_unique_ID)
exists_rows # all accounted for

missing_rows <- start_time_errors_chk %>% # shows no rows that are missing from final_data, which is good as these should not be omitted
  anti_join(final_data %>% select(fish_unique_ID), by = "fish_unique_ID") %>%
  arrange(lake_name, ats_year, trawl_date, trawl_number, fish_unique_ID)
missing_rows # none missing from final
# REVIEW duration_mi_errors_chk                                             ####
#   Define the relevant columns to keep for resolving data issue

# Columns to keep after reducing # of records with time issues using unique() on the meta-data fields alone
keep_cols <- c(
  "trawl_unique_ID",
  "lake_name",
  "lake_code",
  "ats_year",
  "trawl_date", "trawl_number", "depth_m",
  "start_time", "end_time", "duration_mi",
  "species_code",
  "fish_description",
  "source_files",
  "comment")

# 1) Build the working table and tally non-NA lengths per future-unique group
duration_errors_chk <- duration_mi_errors %>%
  # add a per-group tally of non-NA lengths over the *keep_cols* grouping
  add_count(across(all_of(keep_cols)),
            wt = !is.na(fish_length_mm),
            name = "fish_lengths") %>%
  select(
    trawl_unique_ID, fish_unique_ID,
    lake_name, lake_code, ats_year,
    trawl_date, trawl_number, depth_m,
    start_time, end_time, duration_mi,
    species_code, fish_description, fish_lengths,
    fish_length_mm, fish_weight_g,
    fish_id, source_files, comment) %>%
  arrange(lake_name, ats_year, trawl_date, trawl_number, trawl_unique_ID, fish_unique_ID)

# 2) Now make the unique table; the tally of records (fish_lengths) associated with each trawl is preserved
duration_errors_unique_chk <- duration_errors_chk %>%
  select(
    trawl_unique_ID, 
    lake_name, lake_code, ats_year,
    trawl_date, trawl_number, depth_m,
    start_time, end_time, duration_mi,
    species_code, fish_description, fish_lengths,
    source_files, comment) %>%
  unique()  # Result: invalid end_times can all be set to NA

# RESULTS
# Delete all records associated with trawl_unique_id 1991-07-18_67_1_98.109375 which seems to be SAS origin only
# Delete all records associated with trawl_unique_id 1991-09-14_66_99_10 of SAS origin
# Delete record 1991-09-14_66_99_10 of SAS origin

# Compare with final_dataframe to see which rows are present or missing as these should not be omitted
exists_rows <- duration_errors_chk %>%
  semi_join(final_data %>% select(fish_unique_ID), by = "fish_unique_ID") %>%
  left_join(final_data %>% select(fish_unique_ID, data_issues),  # shows data_issues field updated correctly
            by = "fish_unique_ID") %>%
  arrange(lake_name, ats_year, trawl_date, trawl_number, fish_unique_ID)
exists_rows # all accounted for

missing_rows <- duration_errors_chk %>% # shows no rows that are missing from final_data, which is good as these should not be omitted
  anti_join(final_data %>% select(fish_unique_ID), by = "fish_unique_ID") %>%
  arrange(lake_name, ats_year, trawl_date, trawl_number, fish_unique_ID)
missing_rows # none missing from final