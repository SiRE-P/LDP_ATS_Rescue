## --------------------------------------------------------------------------
## Acoustic Target Data Cleanup.R
##
## Purpose: Import acoustic target data files (TARGETyy.DAT files, 1977-2007)
##          for data cleanup and compilation into integrated CSV file(s).
##
##          Raw import data (pre-processing) are output to target_data_RAW_*.csv
##          (where * = date of program execution).
##
##          All processed/cleaned data are output to target_clean_ALL_*.csv. 
##          These data are also segregated into JUVENILE and ADULT acoustic count
##          data as target_clean_JUVENILE_*.csv and target_clean_ADULT_*.csv.
##       
##          Other outputs include: 
##          - a list of exact duplicates that are removed (target_data_exact_duplicates_*.csv);
##          - a list of key field duplicates (same meta-data such as lake, date, but different 
##            target counts) that are retained as likely replicate counts (target_data_exact_duplicates_*.csv).
##          
## Authors: Yuliya Shtymburski (U. Regina); Sandra Emry (UBC); H Stiff (DFO Nanaimo)
## Date:    September 2025
## Notes:   
##          
## --------------------------------------------------------------------------


# Libraries ####

library(dplyr)
library(lubridate)
library(progress)     # for progress bar
library(purrr)
library(stringr)
library(tidyverse)
library(tibble)
library(tictoc)       # get elapsed time 
library(tools)

date_stamp <- substr(format(Sys.time(), "%Y%m%d-%H%M"), 3, 8) # 8 for date only  # Get the current date to timestamp output files

# install.packages("tictoc")

# Functions ####
# function to assign ATS year based on survey date
assign_ats_year <- function(survey_date) {
  if_else(month(survey_date) >= 4,
          year(survey_date),
          year(survey_date) - 1)
} # end function

# function to read input data from TARGET*.DAT files (differs from SE's version in adding line_numbers for trace-ability)
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
# walk(estimated_lines, ~ message(sprintf("NOTE: form feed character encountered in line %d", .x))) # uncomment to log line_numbers with FF character
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
    
    lake <- metadata_block[str_detect(metadata_block, "lake code", negate = TRUE)] %>%
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
    
    acoustic_survey_notes <- metadata_block %>%
      discard(~ str_detect(.x, "lake code|: date|FURUNO|SIMRAD|Gain|^[A-Z]")) %>%
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
    filter(!if_any(c(transect, depth, targets), is.na)) %>% # this drops all records missing in any of three key variables, which seems to happen due to blank lines between input data blocks (HS 250908)
    arrange(source_file, line_number)
  
  return(final_data)
} # end function


# Import TARGET*.DAT files ####

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

write_csv(all_target_data, paste("./output/all_target_data_RAW_", date_stamp, ".csv", sep=""))

raw_data_inventory <- all_target_data %>%
  select(source_file, lake, lake_code, survey_date, sounder_type, sounder_gain = gain, acoustic_survey_notes) %>%
  distinct() %>%
  arrange(source_file, lake, lake_code, survey_date)

write_csv(raw_data_inventory, paste("./output/target_raw_data_INVENTORY_", date_stamp, ".csv", sep=""))


# Error-Check target data ####

# Identify exact duplicate records and save to csv #
target_data_exact_duplicates <- all_target_data %>%
  filter(duplicated(select(., -line_number)))      #  excluding line_number
write_csv(target_data_exact_duplicates, paste("./output/target_data_exact_duplicates_", date_stamp, ".csv", sep=""))

data <- all_target_data %>% 
  
  # filter(lake_code == 1, survey_date == as.Date("1977-09-20")) %>%
  
  # Remove exact duplicates from input data
  distinct(across(-line_number), .keep_all = TRUE) %>%
  
  ### separating depth into min and max
  dplyr::mutate(depth = str_trim(depth)) %>%
  separate(depth, into = c("depth_min", "depth_max"), sep = "\\s*[-–]\\s*", convert = TRUE) %>% 
  
  # moving the extra information from metablock into a comments column 
  separate(lake,
           into = c("lake_name", "survey_comments"),
           sep = "\\s{2,}",
           extra = "merge",
           fill = "right") %>% 
  
  # Assigning target_survey_type as ADULT or JUVENILE (default) -- based on text in survey_comments
  mutate(
    target_survey_code = case_when(
      is.na(survey_comments) | str_trim(survey_comments) == "" ~ 1,
      str_detect(survey_comments, regex("adult", ignore_case = TRUE)) ~ 2,
      TRUE ~ 1),
    target_survey_type = case_when(
      is.na(survey_comments) | str_trim(survey_comments) == "" ~ "JUVENILE",
      str_detect(survey_comments, regex("adult", ignore_case = TRUE)) ~ "ADULT",
      TRUE ~ "JUVENILE")) %>%

  # convert survey_date from character into a data
  mutate(survey_date = ymd(survey_date),
         survey_year = year(survey_date),
         survey_month = month(survey_date),
         survey_comments = ifelse(is.na(survey_comments) | trimws(survey_comments) == "", 
                                  "NO SURVEY COMMENTS", 
                                  survey_comments)) %>% 
  
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
  
##### Addition of ats year
  mutate(
    ats_year = assign_ats_year(survey_date),
    .before = survey_date)  %>% 
  # drop_na()  # I think this is to drop the blank rows in between the collections (SE) -- # but I am not sure why we would want to do this (HS 250903)
  filter(!if_all(c(transect, depth_code, targets), is.na))  # this drops all records missing in three key index variables, which seems to happen due to blank lines between input data blocks (HS 250908)

data <- data %>%  
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

filter_data  <- data %>% filter(if_all(everything(), is.na))   # this captures any and all recs that were NA IN ALL COLUMNS = 0
data_with_na <- data %>% filter(if_any(everything(), is.na))   # this captures any record with an NA in ANY column = 9,674 (HS 250903)
data_no_na   <- data %>% drop_na()                             # drop_na() with no arguments removes any row that has at least one NA in any column = 123,880 = 131,011 - 7,131

# Identify further duplicates on key fields #### 
target_data_keyfield_duplicates <- data %>%
  group_by(lake_code, survey_date, transect, depth_code) %>%  # Key fields: lake_code, survey_date, transect, depth_code
  filter(n() > 1) %>%
  mutate(key_field_replicate = "Replicate exists for this lake, date, transect and depth") %>%   # create new "data issues" column for keyfield replicates
  ungroup()

# View the duplicate records with line numbers
# View(target_data_keyfield_duplicates)
# Export to CSV
write.csv(target_data_keyfield_duplicates, paste("./output/target_data_keyfield_duplicates_", date_stamp, ".csv", sep=""), row.names = FALSE) 

# DO NOT REMOVE key field duplicates from the target_data_exact_dups_removed dataset
  # add in columns from lake_strata (area, length)
lake_strata <- read.csv("./data/lake_strata_lengths.csv") #input the reference file 

merged_data_strata <- data %>%
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

merged_data <- merged_data_strata %>%
  mutate(
    # create data_issues column if missing
    data_issues = "") %>%
  
  mutate(
    total_prop = prop_stickleback + prop_sockeye,
    
    # prop_sockeye / prop_stickleback NAs
    data_issues = case_when(
      is.na(prop_sockeye) & is.na(prop_stickleback) ~
        str_c(data_issues, "Missing proportions: prop_sockeye and prop_stickleback == NA; "),
      TRUE ~ data_issues
    ),
    
    # proportion check 
    data_issues = case_when(
      total_prop > 1.01 ~ str_c(data_issues, "Species proportions do not add to 1; "),
      total_prop < 0.99 ~ str_c(data_issues, "Species proportions do not add to 1; "),
      TRUE ~ data_issues
    ),
  
    # prop_sockeye / prop_stickleback zeros flagged as NA
    data_issues = case_when(
      (prop_sockeye == 0 & prop_stickleback == 0 & targets == 0) ~
        str_c(data_issues, "Targets == 0, prop_sockeye and prop_stickleback set to NA; "),
      TRUE ~ data_issues
    ),
    # Set prop_sockeye and prop_stickleback to NA under the same condition
    prop_sockeye = case_when(
      prop_sockeye == 0 & prop_stickleback == 0 & targets == 0 ~ NA_real_,
      TRUE ~ prop_sockeye
    ),
    prop_stickleback = case_when(
      is.na(prop_sockeye) & prop_stickleback == 0 & targets == 0 ~ NA_real_,
      TRUE ~ prop_stickleback
    ),
    
    # targets < 0 flagged as NA
    data_issues = case_when(
      (targets < 0) ~
        str_c(data_issues, "Targets < 0; "),
      TRUE ~ data_issues
    ),
    
    # Sounder issues 
    data_issues = case_when(
      !(sounder_code %in% c(1, 2)) | is.na(sounder_type) | is.na(sounder_gain) ~ 
        str_c(data_issues, "Sounder data missing; "),
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
    
    # Megin Lake fix
    survey_date = case_when(
      lake_code == 118 & survey_year == 1996 & survey_month == 3 ~ ymd("1996-03-20"),
      TRUE ~ survey_date
    ),
    data_issues = case_when(
      lake_code == 118 & survey_year == 1996 & survey_month == 3 ~
        str_c(data_issues, "Missing or invalid survey_date, assigned from survey comment info; "),
      TRUE ~ data_issues
    ),
    
  # Muriel Lake fix
  survey_date = case_when(
    lake_code == 44 & survey_year == 1996 & survey_month == 3 ~ ymd("1996-03-22"),
    TRUE ~ survey_date
  ),
  data_issues = case_when(
    lake_code == 44 & survey_year == 1996 & survey_month == 3 ~
      str_c(data_issues, "Missing or invalid survey_date, assigned from survey comment info; "),
    TRUE ~ data_issues
  ),

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
  filter(!(lake_code == 228 & survey_date == ymd("2007-02-15"))) %>%
  filter(!(lake_code == 229 & survey_date %in% ymd(c("2004-02-04", "2007-02-15")))) %>%
  mutate(
    data_issues = case_when(
      lake_code == 228 & survey_date == ymd("2007-02-14") ~
        str_c(data_issues, "Duplicate data dated 070215 deleted; "),
      lake_code == 229 & survey_date == ymd("2004-02-14") ~
        str_c(data_issues, "This was a duplicate, other copy deleted; probable true date Feb 14, 2007, from acoustic_survey_notes, data found in TARGET06.DAT; unknown why the target numbers are different than for date 070214; "),
      lake_code == 229 & survey_date == ymd("2007-02-14") ~
        str_c(data_issues, "Duplicate data dated 070215 deleted; "),
      TRUE ~ data_issues)) %>% 
  select(data_issues, source_file, line_number, everything(), -fix_skaha_date, -fix_kca_date, -fix_muriel_date, -fix_megin_date)     # -total_prop, 

# finding and fixing some lake name issues 
cat("Fixing lake name issues...\n") 
merged_data %>% 
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
merged_data <- merged_data %>%
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

merged_data <- merged_data %>% 
  select(-lake.y) %>% 
  rename(lake = lake.x) 

# Count unique lake names per lake_code
lake_check <- merged_data %>%
  group_by(lake_code) %>%
  summarize(
    n_lakes = n_distinct(lake),
    lakes = paste(unique(lake), collapse = ", ")) %>%
  ungroup()

# Show only lake_codes that have more than one name
lake_check %>% filter(n_lakes > 1)

# Identify further duplicates on key fields #### 
merged_data <- merged_data %>%
  group_by(lake_code, survey_date, transect, depth_code) %>%
  mutate(
    key_field_replicate = if_else(                                              # rename this field to data_issues2_key_field_replicate?
      n() > 1,
      "Replicate exists for this lake, date, transect and depth",
      NA_character_)) %>%
  ungroup()

#### NEED TO DEAL WITH WEATHER STILL (SE) --> NO, no need to extract weather info as long as comment line is captured in acoustic_survey_notes (which it is)
#
# unique(merged_data$acoustic_survey_notes) %>% as_tibble()
# 
# merged_data %>%
#   mutate(
#     # Capture everything after the last number+decimal sequence (roughly where the weather notes start)
#     weather = str_extract(acoustic_survey_notes, "(?<=\\d{1,3}\\.\\d{0,2}|\\d{1,3}\\s)[^\\d]*$"),
#     # Trim whitespace and punctuation
#     weather = str_trim(weather),
#     weather = str_remove_all(weather, "[\\.,]$")
#   ) %>% 
#   arrange(acoustic_survey_notes) %>% 
#   select(acoustic_survey_notes, weather) %>% 
#   distinct() %>% View

# Checking for NAs from depth code 
missing_depth_code <- merged_data %>%
  filter(is.na(depth_code)) # GREAT!  # ? Not so sure: quite a few records with missing depths but targets > 0  # FIXED! {hs 250909}

# Missing values check ####
cat("\nMissing Values (NAs) Summary\n")
cat("  Number of records should be zero for all index variables (i.e., lake, date, depth, transect) and target counts (targets), 
  but may not be 0 for proportions and meta-data variables (data_issues, key_field_replicates).\n")
NA_missing_summary <- sapply(merged_data, function(x) sum(is.na(x)))
print(NA_missing_summary)
cat("\n")

# Invalid date check ####
cat("\nInvalid Date Check")
target_date_err_chk <- merged_data %>%
  mutate(
    parsed_date = ymd(survey_date, quiet = TRUE),
    invalid_date_flag = is.na(parsed_date),                  # Flag invalid dates
    year_mismatch_flag = year(parsed_date) != survey_year,
    future_date_flag = parsed_date > Sys.Date(),   
    line_number = row_number()) %>%
    filter(invalid_date_flag | year_mismatch_flag | future_date_flag) %>%
  dplyr::select(source_file, line_number, lake, lake_code, survey_date, depth_code, transect, targets, prop_sockeye, prop_sockeye, survey_comments, acoustic_survey_notes) %>%
  arrange(source_file, line_number, lake, depth_code, transect)

date_issues <- target_date_err_chk 
print(date_issues)
cat("\n")

# Metadata variable range checks ####
cat("\nNumeric Metadata Variable Check: Lake, Dates, Sounder values...")
# Select numeric columns and summarize missing, min, and max
target_metadata <- merged_data %>%
  select(ats_year, lake, survey_date, sounder_type, where(is.numeric)) %>%
  select(-line_number, -targets, -prop_sockeye, -prop_stickleback, -total_prop, -depth_code, -depth_min, -depth_max, -transect, -transect_length, -area) %>%
  mutate(survey_month = as.numeric(as.character(merged_data$survey_month))) %>%
  arrange(ats_year, lake, survey_year, survey_month)

target_metadata_chk <- target_metadata %>%
  distinct() %>%
  arrange(ats_year, lake, survey_year, survey_month)
  
metadata_issues <-target_metadata_chk %>%
  select(where(is.numeric)) %>%
  distinct() %>%
  summarise(across(everything(), list(
    missing = ~sum(is.na(.)),
    min = ~min(., na.rm = TRUE),
    max = ~max(., na.rm = TRUE)))) %>%
  pivot_longer(cols = everything()) %>%
  separate(name, into = c("variable", "stat"), sep = "_(?=[^_]+$)") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  arrange(variable) %>% 
  print()

# main numeric metadata issues include:
#      missing sounder data (code, gain)   <== data appears to be missing the Sounder Gain value; check STRs ?
#      sounder gain = 500 ?                <== program is reading "EY500" which might be a sounder model, not the gain 

# Check uniqueness of sounder_code → sounder_type
cat("\nSounder Code Check (Code 1 = FURUNO; 2 = SIMRAD; ? = BIOSONICS)\n")
sounder_combinations <- target_metadata_chk %>%
  count(sounder_code, sounder_type, name = "n_occurrences") %>%
  arrange(sounder_code, desc(n_occurrences), sounder_type)

total_sounder_recs <- sounder_combinations %>%
  summarize(
    sounder_code = "TOTAL",
    sounder_type = "",
    n_occurrences = sum(n_occurrences)) %>%
  rbind(sounder_combinations) %>%
  arrange(sounder_code) %>%
  print()

cat("\nSounder Code/Type/Gain Error Records\n")
merged_data_final_chk <- merged_data %>%             # flag but do not change sounder_code and sounder_gain issues, errors
  mutate(sounder_issues = "" ) %>%
  mutate(sounder_issues = ifelse(sounder_code == 1 & !str_detect(sounder_type, "FURUNO") |
                                 sounder_code == 2 & !str_detect(sounder_type, "SIMRAD") |
                                !sounder_code %in% c(1, 2) | is.na(sounder_code), 
                                 str_c("Sounder Code or Type error;"), sounder_issues)) %>%
  mutate(sounder_issues = ifelse(sounder_gain > 50 | is.na(sounder_gain), 
                                 str_c(sounder_issues, "Sounder Gain missing or error;"), sounder_issues))

merged_data_sounder_chk <- merged_data_final_chk %>%
  select(sounder_issues, sounder_code, sounder_type, sounder_gain, lake_code, survey_date, source_file, line_number) %>%
  filter(sounder_issues != "") %>%
  distinct(across(-line_number), .keep_all = TRUE) %>%
  arrange(sounder_issues, sounder_code, sounder_type, survey_date)  %>%
  group_split(sounder_issues) %>%
  walk(~{                          # Print each group with a blank line between
    cat("\n---", unique(.x$sounder_issues), "Check Survey Trip Reports (STRs)? ---\n")
    print(.x, n = Inf)
    cat("\n")})

# Variable range checks ####
# main numeric data issues include:
#      total_prop(ortion) sometimes > 1 (and as shown elsewhere, sometimes < 1)
#      targets < 0
# these things are flagged only, not changed
cat("\nRange Check Summary for Transect, Depth, Targets and Proportions values...")
range_issues <- merged_data_final_chk %>%
  filter(
      # transect_length < 0 |
      # area < 0 |
      # depth_min > depth_max |
      # total_prop < 0.99 & total_prop > 0 |
      # total_prop > 1.01 |
      targets < 0 ) %>%
  select(
    data_issues, source_file, line_number, lake, survey_date, 
    targets, prop_sockeye, prop_stickleback, total_prop)
print(range_issues, n = 100) # negative targets 
cat("\n")
write_csv(range_issues, paste("./output/target_data_targets_issues_", date_stamp, ".csv", sep=""))

final_inventory <- merged_data_final_chk %>%
  select(ats_year, lake, lake_code, survey_date, sounder_code, sounder_type, source_file, 
         acoustic_survey_notes, survey_comments) %>%
  distinct() %>%
  arrange(ats_year, lake, lake_code, survey_date)

write_csv(final_inventory, paste("./output/target_clean_INVENTORY_", date_stamp, ".csv", sep=""))

# Output cleaned up data ####
final_data <- merged_data_final_chk %>% 
  mutate(data_issues = str_c(data_issues, " ", sounder_issues)) %>%  # amalgamate all data issues into the data_issues field
  rename(depth_min_m = depth_min,
         depth_max_m = depth_max,
         area_ha = area,
         transect_length_m = transect_length) %>% 
  select(ats_year, data_issues, key_field_replicate, lake_code, lake, survey_date, survey_year, survey_month, depth_code, depth_min_m, 
         depth_max_m, transect, transect_length_m, area_ha, targets, prop_stickleback, prop_sockeye,
         acoustic_survey_notes, survey_comments, sounder_code, sounder_type, sounder_gain, source_file, line_number, everything(), -sounder_issues) %>%
  arrange(ats_year, lake, survey_date, transect, depth_code)

# Segregate adult and juvenile type surveys
adult_target_data <- final_data %>%
  filter(target_survey_type == "ADULT")

juvenile_target_data <- final_data %>%
  filter(target_survey_type == "JUVENILE")

write_csv(final_data, paste("./output/target_clean_ALL_", date_stamp, ".csv", sep=""))
write_csv(adult_target_data, paste("./output/target_clean_ADULT_", date_stamp, ".csv", sep=""))
write_csv(juvenile_target_data, paste("./output/target_clean_JUVENILE_", date_stamp, ".csv", sep=""))

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

