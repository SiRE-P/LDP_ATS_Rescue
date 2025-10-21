# NOTES & Instructions    #### ---------------------------------------------------------------------
## Acoustic Target Data Cleanup.R
##
## Purpose: Import acoustic target data files (TARGETyy.DAT files, ATS-years 1977-2007)
##          for data cleanup and compilation into integrated CSV file(s). 
##          (An ATS-year extends from April 1, yyyy to March 31st, yyyy+1).
##
##          Raw import data (pre-processing) are output to Target_INPUT_data_RAW_(1977_2007)_*.csv
##          and inventoried in Target_INPUT_data_INVENTORY_(1977_2007)_*.csv
##          (where * = date of program execution).
##
##          All processed/cleaned data are output to Target_OUTPUT_data_FINAL_(1977_2007)_*.csv_*.csv
##          and inventoried in Target_OUTPUT_data_INVENTORY_(1977_2007)_*.csv
##          (where * = date of program execution). These data are classified as JUVENILE or ADULT acoustic count
##          data based on survey comment information, and may be distinguished by fields survey_type and survey_type_code.
##       
##          Other outputs (containing CHK in the filename) include: 
##          - a list of EXACT duplicates that are removed (Target_CHK_exact_duplicate_records_*.csv);
##          - a list of key field duplicates (same meta-data such as lake, date, but different 
##            target counts) that are retained as likely replicate counts (Target_CHK_keyfield_duplicate_surveys_*.csv);
##          - a list of records with data issues specific to the targets field (Target_CHK_targets_issues_*.csv).
##          
## Authors: Yuliya Shtymburski (U. Regina); Sandra Emry (UBC); H Stiff (DFO Nanaimo)
## Date:    October 2025
## Notes:   
##          

# LIBRARIES ####

library(dplyr)
library(lubridate)
library(progress)     # for progress bar
library(purrr)
library(stringr)
library(tidyverse)
library(tibble)       # install.packages("tictoc")
library(tictoc)       # get elapsed time 
library(tools)

# INITIALIZE variables ####
date_stamp <- substr(format(Sys.time(), "%Y%m%d-%H%M"), 3, 8) # 8 for date only  # Get the current date to timestamp output files
ats_year_span <- "(1977_2007)_"                               # year span of the data

if (!dir.exists("./output"))  {dir.create("./output")}        # ensure CSV output directory exists
if (!dir.exists("./figures")) {dir.create("./figures")}       # ensure plot output directory exists

# FUNCTIONS ####
# FUNCTION to assign ATS year based on survey date
assign_ats_year <- function(survey_date) {
  if_else(month(survey_date) >= 4,
          year(survey_date),
          year(survey_date) - 1)
} # end function

# FUNCTION to read input data from TARGET*.DAT files (differs from SE's version in adding line_numbers for trace-ability)
parse_target_dat_tracer <- function(filepath) {
  # Read file as a single string to manually handle form feed characters
  raw_text <- read_file(filepath)
  cat("\n")
  print(paste("Processing data in file: ", filepath, sep=""))
  
  # Split into lines using both newline and form feed as delimiters
  raw_lines <- str_split(raw_text, "\\r?\\n|\\f", simplify = FALSE)[[1]]
  # print(raw_lines)
  
  # Generate line numbers based on split
  line_numbers <- seq_along(raw_lines)
  
  # Detect form feed characters and report them
  form_feed_positions <- str_locate_all(raw_text, "\f")[[1]]
  if (nrow(form_feed_positions) > 0) {
    # Estimate line number by counting line breaks before each form feed
    line_breaks <- str_locate_all(raw_text, "\\r?\\n")[[1]][, "start"]
    estimated_lines <- map_int(form_feed_positions[, "start"], ~ sum(line_breaks < .x) + 1)
    #   walk(estimated_lines, ~ message(sprintf("NOTE: form feed character encountered in line %d", .x))) # uncomment to log line_numbers with FF character
  }
  
  line_numbers <- seq_along(raw_lines)       # actual line numbers in the input DAT file
  
  # Identify headers using actual line numbers
  header_indices <- which(str_detect(raw_lines, "depth / transect / targets"))
  # print(paste("Column Headers located at: ", header_indices))
  
  all_data <- list()
  
  for (i in seq_along(header_indices)) {
    header_index <- header_indices[i]
    
    # Metadata block (adjusted to match actual line numbers)
    metadata_start <- max(1, header_index - 8)
    metadata_end <- header_index - 1
    metadata_block <- raw_lines[metadata_start:metadata_end]
    # print(metadata_block)
    
    lake_code <- metadata_block[str_detect(metadata_block, "lake code")] %>%
      str_extract("\\d+(?=\\s*: lake code)") %>%
      as.numeric()
    
    lake <- metadata_block[str_detect(metadata_block, "lake code", negate = TRUE)] %>%  # this captures the whole lake info line, including survey comment
      .[str_detect(., "^[A-Z]")] %>%
      str_trim() %>%
      .[1]
    
    date_str <- metadata_block[str_detect(metadata_block, ": date")] %>%
      str_extract("\\d{6}") %>%
      .[1]
    
    survey_date <- ymd(date_str)
    
    # sounder <- metadata_block[str_detect(metadata_block, "FURUNO|SIMRAD")] %>%
    #   str_extract("FURUNO[^:]*|SIMRAD[^:]*") %>%
    #   str_trim() %>%
    #   .[1]
    
    sounder <- metadata_block[str_detect(metadata_block, "FURUNO|SIMRAD|BIOSONICS")] %>%
      str_extract("^[^:]*") %>%  # extract everything before the first colon
      str_extract("FURUNO[^:]*|SIMRAD[^:]*|BIOSONICS[^:]*") %>%
      str_trim() %>%
      .[1]
    
    gain <- metadata_block[str_detect(metadata_block, "Gain")] %>%
      str_extract("\\d+") %>%
      as.numeric() %>%
      .[1]
    
    acoustic_survey_notes <- metadata_block[-2] %>%                                 # ignore line 2 of the meta-data block (lake name and survey comments, which are already captured above
      # discard(~ str_detect(.x, "lake code|: date|FURUNO|SIMRAD|Gain|^[A-Z]")) %>%   # this was discarding comment lines (6 & 7 in meta-data block) if they started with a capital letter
      discard(~ str_detect(.x, "lake code|: date|FURUNO|SIMRAD|BIOSONICS|Gain")) %>%
      str_trim() %>%
      paste(collapse = " ") %>%
      na_if("")
    
    # Data block
    data_start <- header_index + 1
    data_end <- if (i < length(header_indices)) header_indices[i + 1] - 2 else length(raw_lines)
    
    data_lines <- raw_lines[data_start:data_end]
    data_line_numbers <- line_numbers[data_start:data_end]
    
    # Parse each line, including blanks
    parsed_data <- map2(data_lines, data_line_numbers, ~ {
      fields <- str_split(str_trim(.x), "\\s+")[[1]]
      if (length(fields) >= 5) {
        tibble(
          line_number = .y,
          depth = fields[1],
          transect = as.numeric(fields[2]),
          targets = as.numeric(fields[3]),
          pct_sockeye = as.numeric(fields[4]),
          pct_stickleback = as.numeric(fields[5])
        )
      } else {
        tibble(
          line_number = .y,
          depth = NA_character_,
          transect = NA_real_,
          targets = NA_real_,
          pct_sockeye = NA_real_,
          pct_stickleback = NA_real_
        )
      }
    }) %>%
      bind_rows() %>%
      mutate(
        lake = lake,
        lake_code = lake_code,
        survey_date = survey_date,
        sounder_type = sounder,
        gain = gain,
        acoustic_survey_notes = acoustic_survey_notes,
        source_file = basename(filepath)
      )
    
    all_data[[i]] <- parsed_data
  }
  
  final_data <- bind_rows(all_data) %>%
    select(source_file,
           line_number,
           lake_code,
           lake,
           survey_date,
           depth,
           transect,
           targets,
           pct_sockeye,
           pct_stickleback,
           sounder_type,
           gain,
           acoustic_survey_notes,
           everything()) %>%
    filter(!if_any(c(transect, depth, targets), is.na)) %>% # this drops all records missing in any of three key variables, which may happen due to blank lines between input data blocks (HS 250908)
    arrange(source_file, line_number)
  
  return(final_data)
} # end function


# IMPORT TARGET*.DAT files ####

# list all .dat files in the working directory
dat_files <- list.files(
  path = "data",              # look in the data/ folder
  pattern = "\\.dat$",        # only .dat files
  ignore.case = TRUE,
  full.names = TRUE)          # include full path so read_lines() works

# # list the import DAT files (TARGET*.DAT)
cat("\nAcoustic Target Data files to process: ", dat_files, "\n")

# parse all files and combine into one tibble

pb <- progress_bar$new(       # set up a progress bar
  format = "  Parsing [:bar] :percent (:current/:total files)",
  total = length(dat_files),
  clear = FALSE,
  width = 60,
  show_after = 0)             # show progress bar (immediately)

tic("Compilation time: ")     # get start time

all_target_data <- tibble()

for (file in dat_files) {
  parsed <- parse_target_dat_tracer(file)
  parsed <- parsed %>%
    filter(!if_all(c(transect, depth, targets), is.na)) # drops blank lines (which are missing for these variables)
  all_target_data <- bind_rows(all_target_data, parsed) # append parsed data to all_target_data
  pb$tick()                   # update progress bar
  flush.console()             # force progress bar to appear immediately
  # Sys.sleep(0.1)              # optional: slow down loop to make updates visible
}

cat("\n")
toc()                         # get finish time and post elapsed time
cat("\n")

# tic("Compilation time: ")                                       # Sandra's method
# all_target_data <- dat_files %>%
#   set_names(~ tools::file_path_sans_ext(basename(.x))) %>%
# # map_dfr(parse_target_dat_tracer, .id = "source_file") %>%     # modified function and call to add source file and line no's for traceability (HS 25-09-08)
#
#   map_dfr(function(file) {
#     pb$tick()
#     parse_target_dat_tracer(file)
#   }, .id = "source_file") %>%
#
#   filter(!if_all(c(transect, depth, targets), is.na))           # this drops all records missing in three key index variables, which seems to happen due to blank lines between input data blocks (HS 250908)
# toc()

# Save raw import data and an inventory of surveys to csv...####
write_csv(all_target_data,  paste("./output/Target_INPUT_data_RAW_", ats_year_span, date_stamp, ".csv", sep=""))
# Re-Import raw import data from saved CSV to skip time-consuming import of TARGET*.DAT ####
all_target_data <- read_csv(paste("./output/Target_INPUT_data_RAW_", ats_year_span, "251007", ".csv", sep=""))   # use this if skipping the compilation process, above, with appropriate date of csv

# Output an inventory of unique surveys in the raw data
raw_data_inventory <- all_target_data %>%
  select(source_file, lake, lake_code, survey_date, sounder_type, sounder_gain = gain, acoustic_survey_notes) %>%
  distinct() %>%
  arrange(source_file, lake, lake_code, survey_date)
write_csv(raw_data_inventory, paste("./output/Target_INPUT_data_INVENTORY_", ats_year_span, date_stamp, ".csv", sep=""))



# TIDY target data ####
data <- all_target_data %>%   # target_data_exact_dups_removed %>% 
  
  # filter(lake_code == 1, survey_date == as.Date("1977-09-20")) %>%
  
  ### separating depth into min and max
  dplyr::mutate(depth = str_trim(depth)) %>%
  separate(depth, into = c("depth_min", "depth_max"), sep = "\\s*[-–]\\s*", convert = TRUE) %>% 
  
  # moving the extra information from metablock into a comments column 
  separate(lake,
           into = c("lake_name", "survey_comments"),
           sep = "\\s{2,}",
           extra = "merge",
           fill = "right") %>% 
  
  # Assigning target_survey_type as ADULT or JUVENILE (default) -- based on text="ADULT SURVEY" in survey_comments (ignore other references to adults present, not present, etc)
  mutate(
    target_survey_code = case_when(
      is.na(survey_comments) | str_trim(survey_comments) == "" ~ 1,
      str_detect(survey_comments, regex("adult survey", ignore_case = TRUE)) ~ 2,
      TRUE ~ 1),
    target_survey_type = case_when(
      is.na(survey_comments) | str_trim(survey_comments) == "" ~ "JUVENILE",
      str_detect(survey_comments, regex("adult survey", ignore_case = TRUE)) ~ "ADULT",
      TRUE ~ "JUVENILE")) %>%
  
  # convert survey_date from character into a data
  mutate(survey_date = ymd(survey_date),
         survey_year = year(survey_date),
         survey_month = month(survey_date),
         survey_comments = ifelse(is.na(survey_comments) | trimws(survey_comments) == "", 
                                  "NA", survey_comments)) %>% 
  
  ### renaming sockeye/stickleback columns
  rename(prop_sockeye = pct_sockeye) %>% 
  rename(prop_stickleback = pct_stickleback) %>%  
  
  # adding in depth codes according to min and max depths 
  mutate(depth_code = case_when( 
    depth_min == 2  & depth_max == 5  ~ 1,
    depth_min == 3  & depth_max == 5  ~ 2,
    depth_min == 5  & depth_max == 10 ~ 3,
    depth_min == 10 & depth_max == 15 ~ 4,
    depth_min == 15 & depth_max == 20 ~ 5,
    depth_min == 20 & depth_max == 30 ~ 6,
    depth_min == 30 & depth_max == 40 ~ 7,
    depth_min == 40 & depth_max == 50 ~ 8,
    depth_min == 50 & depth_max == 60 ~ 9,
    depth_min == 60 & depth_max == 70 ~ 10,
    depth_min == 70 & depth_max == 80 ~ 11,
    depth_min == 80 & depth_max == 90 ~ 12,
    depth_min == 90 & depth_max == 100~ 13)) %>%
  
  # separating sounder type and sounder code
  # extract(sounder_type, into = c("sounder_type", "sounder_code"), regex = "^(\\S+)\\s+(.*)$") %>% 
  # mutate(sounder_code = as.numeric(sounder_code)) %>%
  # rename(sounder_gain = gain) %>% 
  
  extract(
    sounder_type,
    into = c("sounder_type", "sounder_code"),
    regex = "^(.*\\D)(\\d+)$"
  ) %>%
  mutate(sounder_code = as.numeric(sounder_code)) %>%
  rename(sounder_gain = gain) %>%
  
  # renaming columns 
  rename(lake = lake_name) %>% 
  
  # Addition of ats year
  mutate(
    ats_year = assign_ats_year(survey_date),
    .before = survey_date)  %>% 
  # drop_na()  # I think this is to drop the blank rows in between the collections (SE) -- # but I am not sure why we would want to do this (HS 250903)
  filter(!if_all(c(transect, depth_code, targets), is.na))  # this drops all records missing in three key index variables, which seems to happen due to blank lines between input data blocks (HS 250908)

tidy_data <- data %>%  
  #rearranging column order
  select(source_file,
         line_number,
         lake_code,
         lake,
         target_survey_code,
         target_survey_type,
         survey_date,
         survey_year,
         survey_month,
         depth_code,
         depth_min,
         depth_max,
         transect,
         targets,
         prop_sockeye,
         prop_stickleback,
         sounder_type,
         sounder_code,
         sounder_gain,
         acoustic_survey_notes,
         survey_comments,
         everything()) %>% 
  arrange(source_file, line_number, ats_year, lake, survey_date, transect, depth_code)

filter_data  <- tidy_data %>% filter(if_all(everything(), is.na))   # this captures any and all recs that were NA IN ALL COLUMNS = 0
data_with_na <- tidy_data %>% filter(if_any(everything(), is.na))   # this captures any record with an NA in ANY column = 20,099 (HS 251006)
data_no_na   <- tidy_data %>% drop_na()                             # drop_na() with no arguments removes any row that has at least one NA in any column = 123,880 = 131,011 - 7,131

#   Merge in parameters from lake_strata (area, length) ####
lake_strata <- read.csv("./data/lake_strata_lengths.csv") #input the reference file 

merged_data_strata <- tidy_data %>%
  left_join(
    lake_strata %>%
      rename(lake = lk.name, lake_code = lk.code, transect = Transect) %>%
      mutate(transect = str_extract(transect, "\\d+") %>% as.numeric()),
    by = c("lake_code", "depth_min", "depth_max", "transect"), 
    relationship = "many-to-one"
  ) %>% 
  rename(area = Area) %>% 
  rename(transect_length = Length) %>% 
  relocate(lake.y, .after = lake.x)

#   Error checking ####
merged_data_with_issues <- merged_data_strata %>%
  mutate(
    # create data_issues column if missing
    data_issues = "") %>%
  
  # compare ats_year with source_file year derived from TARGETyy.DAT filename
  mutate(
    sourcefile_year  = as.numeric(substr(source_file, 7, 8)),                   # get 2-digit year from TARGETyy.DAT and... 
    sourcefile_year  = ifelse(sourcefile_year > 70, sourcefile_year + 1900, sourcefile_year + 2000),    # convert to year
    source_year_err  = ats_year != sourcefile_year,                             # "Source file / survey_date mis-match"), # survey in wrong TARGETyy.DAT file
    data_issues = case_when(
      ats_year != sourcefile_year ~
        str_c(data_issues, "Source_file/survey_date mis-match; "),
      TRUE ~ data_issues)) %>%
  
  # do some error checks and flag any issues in data_issues field...   
  mutate(
    
    # prop_sockeye & prop_stickleback not provided, though targets exist
    data_issues = case_when(
      is.na(prop_sockeye) & is.na(prop_stickleback) & targets > 0 ~
        str_c(data_issues, "Missing species proportions (NA) but targets > 0; "),
      TRUE ~ data_issues
    ),
    
    # prop_sockeye / prop_stickleback zeros flagged as NA
    data_issues = case_when(
      # (prop_sockeye == 0 & prop_stickleback == 0 & targets == 0) ~
      targets == 0 & (is.numeric(prop_sockeye) | is.numeric(prop_stickleback)) ~
        str_c(data_issues, "Prop_sockeye and prop_stickleback set to NA because targets == 0; "),
      TRUE ~ data_issues
    ),
    # Set prop_sockeye and prop_stickleback to NA under the same condition
    prop_sockeye = case_when(
      # prop_sockeye == 0 & prop_stickleback == 0 & targets == 0 ~ NA_real_,
      targets == 0 & (is.numeric(prop_sockeye) | is.numeric(prop_stickleback)) ~ NA_real_,
      TRUE ~ prop_sockeye
    ),
    prop_stickleback = case_when(
      # is.na(prop_sockeye) & prop_stickleback == 0 & targets == 0 ~ NA_real_,
      targets == 0 & (is.numeric(prop_sockeye) | is.numeric(prop_stickleback)) ~ NA_real_,
      TRUE ~ prop_stickleback
    ),
    
    total_prop = prop_stickleback + prop_sockeye,
    
    # proportion check 
    data_issues = case_when(
      is.numeric(total_prop) & (total_prop < 0.99 | total_prop > 1.01) ~ 
        str_c(data_issues, "Species proportions do not add to 1; "),
      TRUE ~ data_issues
    ),
    
    # targets < 0 
    data_issues = case_when(
      (targets < 0) ~
        str_c(data_issues, "Targets < 0; "),
      TRUE ~ data_issues
    ),
    
    # transect_length = 0 (lake bottom) but targets > 0
    data_issues = case_when(
      transect_length == 0 & targets > 0 ~
        str_c(data_issues, "Transect Length = 0 but #Targets > 0; "),
      TRUE ~ data_issues
    ),
    
    # Sounder issues - these are better flagged below
    # data_issues = case_when(
    #   !(sounder_code %in% c(1, 2)) | is.na(sounder_type) | is.na(sounder_gain) ~ 
    #     str_c(data_issues, "Sounder data missing; "),
    #   TRUE ~ data_issues
    # ),
    
    # GCL data fix for incorrect proportions at certain depths (survey_date 2006/02/16)
    fix_gcl_props = lake_code == 1 & source_file == "TARGET05.DAT" & survey_date == ymd("2006-02-16"),
    
    prop_sockeye = case_when(
      fix_gcl_props & (depth_code == 3 | depth_code == 4) ~ 0.92,
      TRUE ~ prop_sockeye),
    prop_stickleback = case_when(
      fix_gcl_props & (depth_code == 3 | depth_code == 4) ~ 0.08,
      TRUE ~ prop_stickleback),
    
    data_issues = case_when(
      fix_gcl_props & (depth_code == 3 | depth_code == 4) ~ str_c(data_issues, "Revised proportions (was 0.73 sox, 0.27 stix) based on TARGET05ADJ.DAT file; "),
      TRUE ~ data_issues
    ),
    
    prop_sockeye = case_when(
      fix_gcl_props & (depth_code == 5) ~ 0.74,
      TRUE ~ prop_sockeye),
    prop_stickleback = case_when(
      fix_gcl_props & (depth_code == 5) ~ 0.26,
      TRUE ~ prop_stickleback),
    
    data_issues = case_when(
      fix_gcl_props & (depth_code == 5)~ str_c(data_issues, "Revised proportions (was 0.40 sox, 0.60 stix) based on TARGET05ADJ.DAT file; "),
      TRUE ~ data_issues
    ),
    
    # GCL data fix for incorrect proportions at certain depths (survey_date 2003/01/15)
    fix_gcl_props = lake_code == 1 & source_file == "TARGET02.DAT" & survey_date == ymd("2003-01-15"),
    
    prop_sockeye = case_when(
      fix_gcl_props & (depth_code == 1 | depth_code == 3) ~ 0.76,               # no changes for depth_code == 2
      TRUE ~ prop_sockeye),
    prop_stickleback = case_when(
      fix_gcl_props & (depth_code == 1 | depth_code == 3) ~ 0.24,
      TRUE ~ prop_stickleback),
    
    data_issues = case_when(
      fix_gcl_props & (depth_code == 1 | depth_code == 3) ~ str_c(data_issues, "Revised proportions (was 0.23 & 0.92 sox, 0.77 & 0.08 stix) based on TARGET02ADJ.DAT file; "),
      TRUE ~ data_issues
    ),
    
    prop_sockeye = case_when(
      fix_gcl_props & (depth_code == 4) ~ 0.86,
      TRUE ~ prop_sockeye),
    prop_stickleback = case_when(
      fix_gcl_props & (depth_code == 4) ~ 0.14,
      TRUE ~ prop_stickleback),
    
    data_issues = case_when(
      fix_gcl_props & (depth_code == 4)~ str_c(data_issues, "Revised proportions (was 0.23 sox, 0.77 stix) based on TARGET02ADJ.DAT file; "),
      TRUE ~ data_issues
    ),
    
    prop_sockeye = case_when(
      fix_gcl_props & (depth_code == 5) ~ 0.94,
      TRUE ~ prop_sockeye),
    prop_stickleback = case_when(
      fix_gcl_props & (depth_code == 5) ~ 0.06,
      TRUE ~ prop_stickleback),
    
    data_issues = case_when(
      fix_gcl_props & (depth_code == 5)~ str_c(data_issues, "Revised proportions (was 0.83 sox, 0.17 stix) based on TARGET02ADJ.DAT file; "),
      TRUE ~ data_issues
    ),
    
    prop_sockeye = case_when(
      fix_gcl_props & (depth_code == 6 | depth_code == 7) ~ 1.00,
      TRUE ~ prop_sockeye),
    prop_stickleback = case_when(
      fix_gcl_props & (depth_code == 6 | depth_code == 7) ~ 0.00,
      TRUE ~ prop_stickleback),
    
    data_issues = case_when(
      fix_gcl_props & (depth_code == 6 | depth_code == 7)~ str_c(data_issues, "Revised proportions (was 0.99 sox, 0.01 stix) based on TARGET02ADJ.DAT file; "),
      TRUE ~ data_issues
    ),
    
    
    # SKAHA Lake date fix for invalid date (00/09/31)
    fix_skaha_date = lake_code == 241 & source_file == "TARGET00.DAT" & is.na(survey_date),
    survey_date = case_when(
      fix_skaha_date ~ ymd("2000-10-01"),
      TRUE ~ survey_date),
    survey_year = year(survey_date),
    survey_month = month(survey_date),
    ats_year = assign_ats_year(survey_date),
    
    data_issues = case_when(
      fix_skaha_date ~ str_c(data_issues, "Missing or invalid survey_date (31-Sep-00), assigned (01-Oct-00) from survey comment info; "),
      TRUE ~ data_issues
    ),
    
    # KCA Lake date fix for invalid date (850631)
    fix_kca_date = lake_code == 40 & source_file == "TARGET85.DAT" & is.na(survey_date),
    survey_date = case_when(
      fix_kca_date ~ ymd("1985-07-01"),
      TRUE ~ survey_date),
    survey_year = year(survey_date),
    survey_month = month(survey_date),
    ats_year = assign_ats_year(survey_date),
    
    data_issues = case_when(
      fix_kca_date ~ str_c(data_issues, "Missing or invalid survey_date (31-Jun-85), assigned (01-Jul-85) from survey comment info; "),
      TRUE ~ data_issues
    ),
    
    # Muriel Lake date fix for invalid date (96/03/--)
    fix_muriel_date = lake_code == 44 & source_file == "TARGET95.DAT" & is.na(survey_date),
    survey_date = case_when(
      fix_muriel_date ~ ymd("1996-03-22"),
      TRUE ~ survey_date),
    survey_year = year(survey_date),
    survey_month = month(survey_date),
    ats_year = assign_ats_year(survey_date),
    
    data_issues = case_when(
      fix_muriel_date ~ str_c(data_issues, "Missing or invalid survey_date (96/03/--), assigned (960322) from survey comment info; "),
      TRUE ~ data_issues
    ),
    
    # Megin Lake date fix for invalid date (96/03/--)
    fix_megin_date = lake_code == 118 & source_file == "TARGET95.DAT" & is.na(survey_date),
    survey_date = case_when(
      fix_megin_date ~ ymd("1996-03-20"),
      TRUE ~ survey_date),
    survey_year = year(survey_date),
    survey_month = month(survey_date),
    ats_year = assign_ats_year(survey_date),
    
    data_issues = case_when(
      fix_megin_date ~ str_c(data_issues, "Missing or invalid survey_date (96/03/--), assigned (960320) from survey comment info; "),
      TRUE ~ data_issues
    ),
    
    # # Megin Lake fix - commented out as now redundant
    # survey_date = case_when(
    #   lake_code == 118 & survey_year == 1996 & survey_month == 3 ~ ymd("1996-03-20"),
    #   TRUE ~ survey_date
    # ),
    # data_issues = case_when(
    #   lake_code == 118 & survey_year == 1996 & survey_month == 3 ~
    #     str_c(data_issues, "Missing or invalid survey_date, assigned from survey comment info; "),
    #   TRUE ~ data_issues
    # ),
    
    # # Muriel Lake fix - commented out as now redundant
    # survey_date = case_when(
    #   lake_code == 44 & survey_year == 1996 & survey_month == 3 ~ ymd("1996-03-22"),
    #   TRUE ~ survey_date
    # ),
    # data_issues = case_when(
    #   lake_code == 44 & survey_year == 1996 & survey_month == 3 ~
    #     str_c(data_issues, "Missing or invalid survey_date, assigned from survey comment info; "),
    #   TRUE ~ data_issues
    # ),
    
    # Tatsemenie fix
    ats_year = case_when(
      lake_code == 66 & survey_date == ymd("1992-08-01") ~ 1992L,
      TRUE ~ ats_year
    ),
    data_issues = case_when(
      lake_code == 66 & survey_date == ymd("1992-08-01") ~
        str_c(data_issues, "Data erroneously stored in TARGET95.DAT, ats_year updated from survey_date and processed_date; "),
      TRUE ~ data_issues
    ),
    # Year vs ats_year 
    data_issues = case_when(
      (survey_year == ats_year & survey_month > 3) |
        (survey_year == ats_year + 1 & survey_month < 4) ~ data_issues,
      TRUE ~ str_c(data_issues, "Survey date not in ATS Year; ")
    )
  ) %>%
  # Owikeno removals
  # filter(!(lake_code == 228 & survey_date == ymd("2007-02-15"))) %>%
  # filter(!(lake_code == 229 & survey_date %in% ymd(c("2004-02-04", "2007-02-15")))) %>%
  
  mutate(
    data_issues = case_when(
      lake_code == 228 & survey_date == ymd("2007-02-14") ~
        str_c(data_issues, "Duplicate data dated 070215 deleted; "),
      lake_code == 229 & survey_date == ymd("2004-02-14") ~
        str_c(data_issues, "This was a duplicate, other copy deleted; probable true date Feb 14, 2007, from acoustic_survey_notes, data found in TARGET06.DAT; unknown why the target numbers are different than for date 070214; "),
      lake_code == 229 & survey_date == ymd("2007-02-14") ~
        str_c(data_issues, "Duplicate data dated 070215 deleted; "),
      TRUE ~ data_issues)) %>% 
  select(data_issues, source_file, line_number, everything(), -fix_skaha_date, -fix_kca_date, -fix_muriel_date, -fix_megin_date, -fix_gcl_props)     # -total_prop, 

# finding and fixing some lake name issues 
cat("Fixing lake name issues...\n") 
merged_data_with_issues %>% 
  select(lake.x, lake.y) %>% 
  filter(lake.x != lake.y) %>% 
  unique() %>% 
  arrange(lake.x) # %>% 
# print(n = Inf)

# Reference table of old and new names
lake_lookup <- tibble::tibble(
  lake_old = c("GCL", "AWUN", "HENDERSON", "Henderson LK", "EDEN", 
               "BONILLA", "DEVON", "HOBITON", "KITLOPE", "LOWE", "LongA", "LongB", 
               "MERCER", "MURIEL", "MURIEL LAKE", "NIMPKISH", "WOSS", "ALISTAIR", 
               "CURTIS", "FRED WRIGHT", "IAN", "YAKOUN", "CHEEWHAT", "SPROAT", 
               "JANSEN", "MUCHALAT", "PORT JOHN", "GREAT CENTRAL", "Alistair Lk", 
               "KCA", "KMA", "Kennedy L (Clay)", "Kennedy L (Main)"),
  lake_new = c("Great Central Lk", "Awun Lk", "Henderson Lk", "Henderson Lk", 
               "Eden Lk", "Bonilla Lk", "Devon Lk", "Hobiton Lk", "Kitlope Lk", 
               "Lowe Lk", "Long Lk (A)", "Long Lk (B)", "Mercer Lk", "Muriel Lk", 
               "Muriel Lk", "Nimpkish Lk", "Woss Lk", "Alastair Lk", "Curtis Lk", 
               "Fred Wright Lk", "Ian Lk", "Yakoun Lk", "Cheewhat Lk", "Sproat Lk", 
               "Jansen Lk", "Muchalat Lk", "Port John Lk", "Great Central Lk", "Alastair Lk", 
               "Kennedy Lk (Clayoquot Arm)", "Kennedy Lk (Main Arm)", 
               "Kennedy Lk (Clayoquot Arm)", "Kennedy Lk (Main Arm)"))

# Join and replace
merged_data <- merged_data_with_issues %>%
  left_join(lake_lookup, by = c("lake.x" = "lake_old")) %>%
  mutate(lake.x = coalesce(lake_new, lake.x)) %>%
  select(-lake_new)

# merged_data$lake.x %>% unique() %>% sort(.)
# n_distinct(merged_data$lake_code)

cat("\nFinal unique lake_code and lake_name combinations...\n") 
tallied_data <- merged_data %>%
  group_by(lake_code, lake.x) %>%
  tally()
print(tallied_data, n = Inf)
cat("\nTotal target data records:", sum(tallied_data$n), "\n") # Print the total record count

# different lake codes --> YES, leave as is for now with separate lake_codes despite same lake_name [HS 250909]
# Heydon_2005 and Heydon Lk
# Nahwitti_2005 and Nahwitti Lk
# Osoyoos_2005 and ? probably Osoyoos Lk (N)
# Phillips_2005 and Phillips Lk
# Quatse_2005 and Quatse Lk
# Skaha_2007 and Skaha Lk

merged_tidy_data <- merged_data %>% 
  select(-lake.y) %>% 
  rename(lake = lake.x) 

# Count unique lake names per lake_code
lake_check <- merged_tidy_data %>%
  group_by(lake_code) %>%
  summarize(
    n_lakes = n_distinct(lake),
    lakes = paste(unique(lake), collapse = ", ")) %>%
  ungroup()

# Show only lake_codes that have more than one name
lake_check %>% filter(n_lakes > 1)

# DUPLICATES ####
#   Identify exact duplicate records (on all fields) for removal ####
target_data_exact_duplicates <- merged_tidy_data %>%             # was all_target_data %>%
  filter(duplicated(select(., -line_number, -source_file, -data_issues))) %>%  # all fields, excluding line_number and source_file
  arrange(lake, lake_code, survey_date, source_file, line_number, transect) %>%
  select( lake, lake_code, survey_date, source_file, line_number, transect, depth_code, everything()) # re-order
# View(target_data_exact_duplicates)
# Export to CSV
write_csv(target_data_exact_duplicates, paste("./output/Target_CHK_exact_duplicate_records_", date_stamp, ".csv", sep=""))

cat("List of EXACT duplicate records (same lake, same survey date)...\n") 
target_data_exact_dups_inventory <- target_data_exact_duplicates %>%
  select(lake, lake_code, survey_date, transect) %>% unique() %>%  
  print(n = Inf)
cat("\n")

# Remove exact duplicates from input data
target_data_exact_dups_removed <- merged_tidy_data %>%                          # was all_target_data %>%
  distinct(across(!all_of(c("line_number", "source_file", "data_issues"))), .keep_all = TRUE) %>%
  arrange(lake, lake_code, survey_date, transect, depth_code)
  # results: merged_tidy_data - exact_dups = target_data_exact_dups_removed = 129,529 -199 = 129,330 obs 

#   Identify further POTENTIAL duplicates that are duplicates on key fields #### 
target_data_keyfield_duplicates <- target_data_exact_dups_removed %>%
  group_by(lake_code, survey_date, transect, depth_code) %>%  # Key fields: lake_code, survey_date, transect, depth_code
  filter(n() > 1) %>%                                         # Filter for export any records where more than one survey exists for same key fields
  mutate(key_field_replicate = "Replicate exists for this lake, date, transect and depth") %>%   # add new data issues column for keyfield replicates
  ungroup()

#   Export the duplicate records with line numbers but DO NOT REMOVE key field duplicates from the data until after inspection
write.csv(target_data_keyfield_duplicates, paste("./output/Target_CHK_keyfield_duplicate_surveys_", date_stamp, ".csv", sep=""), row.names = FALSE) 

#   Repeat key-field duplicates check but do not filter, just flag the situation in key_field_replicate column 
target_data_keyfield_dups_flagged <- target_data_exact_dups_removed %>%
  group_by(lake_code, survey_date, transect, depth_code) %>%  # Key fields: lake_code, survey_date, transect, depth_code
  mutate(key_field_replicate = ifelse(n() > 1, "Replicate exists for this lake, date, transect and depth","")) %>%   # create new "data issues" column for keyfield replicates
  ungroup()

#   Identify further POTENTIAL duplicates based on sequential survey_date pairs for the same lake ####
lake_surveys_unique <- target_data_keyfield_dups_flagged %>% # was <-data %>%
  select(ats_year, lake, lake_code, survey_date, sounder_code, sounder_type, source_file, 
         acoustic_survey_notes, acoustic_survey_comments = survey_comments) %>%
  distinct() %>%
  arrange(ats_year, lake, lake_code, survey_date)

lake_survey_sequential_pairs <- lake_surveys_unique %>%
  group_by(lake) %>%
  arrange(survey_date, .by_group = TRUE) %>%
  mutate(prev_date = lag(survey_date),
         next_date = lead(survey_date),
         is_sequential = (as.numeric(survey_date - prev_date) == 1 |
                            as.numeric(next_date - survey_date) == 1)) %>%
  filter(is_sequential) %>%
  arrange(lake, survey_date) %>%
  mutate(pair_start = (as.numeric(survey_date - prev_date) != 1),
         sequential_survey_pair_id = cumsum(pair_start)) %>%
  mutate(sequential_survey_pair_id = ifelse(is.na(sequential_survey_pair_id), 99, sequential_survey_pair_id)) %>%
  select(-prev_date, -next_date, -is_sequential, -pair_start) %>%
  ungroup()

# Select key columns from lake_survey_sequential_pairs
lake_survey_keys <- lake_survey_sequential_pairs %>%
  select(ats_year, lake_code, survey_date, sounder_type, sequential_survey_pair_id)

# Perform a semi-join to keep only matching records from lake_surveys_unique
lake_sequential_survey_data <- target_data_keyfield_dups_flagged %>% # was <-data %>%
  semi_join(lake_survey_keys, by = c("ats_year", "lake_code", "survey_date")) %>%
  mutate(sequential_date_replicate = "Potential duplicate survey near this date") %>%
  select(ats_year, lake, lake_code, survey_date, sounder_type, sounder_code, transect, depth_code, targets, prop_sockeye, target_survey_type, source_file, line_number, 
         survey_comments, acoustic_survey_notes, sequential_date_replicate) %>% 
  arrange(lake, survey_date)

# export all surveys with same lake and sequential dates to csv for survey comparison via Excel Pivot tables
write_csv(lake_sequential_survey_data, paste("./output/Target_CHK_sequential_surveys_", date_stamp, ".csv", sep=""))

# Flag sequential surveys without merging (due to possible many-to-many match-merge)
lake_sequential_flagged <- target_data_keyfield_dups_flagged %>%
  rowwise() %>%
  mutate(sequential_date_replicate = if_else(
    any(lake_code == lake_survey_sequential_pairs$lake_code &
          survey_date == lake_survey_sequential_pairs$survey_date),
    "Potential duplicate survey due to sequential date!",
    NA_character_
  )) %>%
  ungroup()

