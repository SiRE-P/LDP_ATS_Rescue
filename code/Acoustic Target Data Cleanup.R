# ACOUSTIC TARGET DATA CLEANUP.R  NOTES    #### ---------------------------------------------------------------------
##
## Acoustic Target Data Cleanup.R
##
## Purpose: Import acoustic target data files (TARGETyy.DAT files, ATS-years 1977-2007)
##          for data cleanup and compilation into integrated CSV file(s). 
##          (An ATS-year extends from April 1, yyyy to March 31st, yyyy+1).
##
##          Raw imported data (pre-processing) are output to Target_INPUT_data_RAW_(1977_2007)_*.csv
##                                        and inventoried in Target_INPUT_data_INVENTORY_(1977_2007)_*.csv
##                                        (where * = date of program execution).
##
##          All cleaned data (post-processing) are output to Target_OUTPUT_data_FINAL_(1977_2007)_*.csv
##                                        and inventoried in Target_OUTPUT_data_INVENTORY_(1977_2007)_*.csv
##                                        (where * = date of program execution). 
##
##          Cleaned data are classified as JUVENILE or ADULT acoustic count data based on survey comment 
##          information, and may be distinguished by fields survey_type and survey_type_code.
##       
##          Other data check outputs (containing CHK in the filename) include: 
##          - a list of EXACT duplicates that are removed (Target_CHK_exact_duplicate_records_*.csv);
##          - a list of KEY FIELD duplicates (same lake, date)
##            that should be cross-checked to determine whether they are actual duplicates, or
##            replicate surveys or counts (Target_CHK_keyfield_duplicate_surveys_*.csv);
##          - a list of SEQUENTIAL DATE same-lake replicates that should also be checked (Target_CHK_sequential_surveys_*.csv);
##          - a list of records with data issues specific to the targets field (Target_CHK_targets_issues_*.csv); and
##          - a comparison of the inventory of surveys that were in the raw input data versus those in the final inventory
##            classified to source (Target_CHK_RAW_vs_CLEAN_surveys_*.csv).
##          
## Authors: Yuliya Shtymburski (U. Regina); Sandra Emry (UBC); H Stiff (DFO Nanaimo)
## Date:    October 2025
##
## Notes:   The IMPORT TARGET*.DAT files section may be commented out to skip the time-consuming (5 minutes) data-import process
##          if that process has previously been successfully completed.  Note that this may require revision of the FILE_NAME in the 
##          Re-IMPORT RAW IMPORT DATA section to ensure code has access to the raw import data previously saved to CSV.
##          

# LIBRARIES ####
library(beepr)
library(dplyr)
library(lubridate)
library(progress)     # for progress bar
library(purrr)
library(stringr)
library(tibble)       # install.packages("tictoc")
library(tictoc)       # get elapsed time 
library(tidyverse)
library(tools)

# INITIALIZE variables ####
start_time <- Sys.time()                                                        
date_stamp <- substr(format(Sys.time(), "%Y%m%d-%H%M"), 3, 8) # 8 for date only  # Get the current date to timestamp output files
ats_year_span <- "(1977_2007)_"                               # year span of the data

if (!dir.exists("./output"))  {dir.create("./output")}        # ensure CSV output directory exists
if (!dir.exists("./figures")) {dir.create("./figures")}       # ensure plot output directory exists

# FUNCTIONS ####
#   FUNCTION to assign ATS year based on survey date
assign_ats_year <- function(survey_date) {
  if_else(month(survey_date) >= 4,
          year(survey_date),
          year(survey_date) - 1)
} # end function

#   FUNCTION to make a sound
make_a_sound <- function(noise, pause) {
  beep(sound = noise)
  Sys.sleep(pause)
  beep(sound = noise)}

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


# IMPORT TARGET*.DAT files                                      ## COMMENT OUT to skip TARGET.DAT import process ####

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

#   Save raw import data and an inventory of surveys to csv...  ## COMMENT OUT to skip TARGET.DAT import process ####
write_csv(all_target_data,  paste("./output/Target_INPUT_data_RAW_", ats_year_span, date_stamp, ".csv", sep=""))
write_csv(all_target_data,  paste("./output/Target_INPUT_data_RAW_", ats_year_span, "GENERIC.csv",      sep="")) # save copy for import to Pivot workbook
#   Re-Import raw import data (assign FILE_NAME if necessary!) from saved CSV to skip time-consuming import of TARGET*.DAT ####
# all_target_data <- read_csv(paste("./output/Target_INPUT_data_RAW_", ats_year_span, date_stamp, ".csv", sep=""))   # use this if skipping the compilation process, above, using csv labelled with current date-stamp
all_target_data <- read_csv(paste("./output/Target_INPUT_data_RAW_", ats_year_span, "GENERIC.csv"     , sep=""))   # use this if skipping the compilation process, above, using csv labelled with current date-stamp

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

  # mutate(
  #   target_survey_code = case_when(
  #     is.na(survey_comments) | str_trim(survey_comments) == "" ~ 1,
  #     str_detect(survey_comments, regex("adult survey", ignore_case = TRUE)) ~ 2,
  #     TRUE ~ 1),
  #   target_survey_type = case_when(
  #     is.na(survey_comments) | str_trim(survey_comments) == "" ~ "JUVENILE",
  #     str_detect(survey_comments, regex("adult survey", ignore_case = TRUE)) ~ "ADULT",
  #     TRUE ~ "JUVENILE")) %>%

  mutate(
    target_survey_code = case_when(
      (is.na(survey_comments) | str_trim(survey_comments) == "") &
      (is.na(acoustic_survey_notes) | str_trim(acoustic_survey_notes) == "") ~ 1,
      
      str_detect(survey_comments,       regex("adult survey|adult count|adult estimate|adult target estimate|adult day survey|large target count", ignore_case = TRUE)) |
      str_detect(acoustic_survey_notes, regex("adult survey|adult count|adult estimate|adult target estimate|adult day survey|large target count", ignore_case = TRUE)) ~ 2,
      TRUE ~ 1),
    
    target_survey_type = case_when(
      (is.na(survey_comments) | str_trim(survey_comments) == "") &
      (is.na(acoustic_survey_notes) | str_trim(acoustic_survey_notes) == "") ~ "JUVENILE",
      
      str_detect(survey_comments,       regex("adult survey|adult count|adult estimate|adult target estimate|adult day survey|large target count", ignore_case = TRUE)) |
      str_detect(acoustic_survey_notes, regex("adult survey|adult count|adult estimate|adult target estimate|adult day survey|large target count", ignore_case = TRUE)) ~ "ADULT",
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


# Tabulate combinations of sounder_type and sounder_code
tidy_data %>%
  count(sounder_type, sounder_code) %>%
  arrange(desc(n))


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

#   Data check and known error processing ####
#   Filter out duplicate survey data identified in LDP Data Issues.PDF ####
#   See "LDP Data Issues.PDF / section Key-Field Replicate Investigations" for details
target_data_duplicates_to_remove <- merged_data_strata %>%
  arrange(lake_code, survey_date, transect, depth_code) %>%
  filter((lake_code == 6   & survey_date == as.Date("1990-02-22") & source_file == "TARGET90.DAT") | # Hobiton
         (lake_code == 226 & survey_date == as.Date("1994-08-25") & source_file == "TARGET93.DAT") | # Johanson 
       # (lake_code == 40  & survey_date == as.Date("1985-01-23") & source_file == "TARGET85.DAT") | # 3 KCA (Kenn-Clay) surveys this date - supported by three entries with this date for KCA in ALL_YEARS_MASTER_ATS_SUMMARY.xlsx - leave intact [251025] 
         (lake_code == 229 & survey_date == as.Date("2004-02-04") & source_file == "TARGET06.DAT") | # Owikeno (B)
         (lake_code == 225 & survey_date == as.Date("1994-08-23") & source_file == "TARGET93.DAT") | # Sustut
         (lake_code == 64  & survey_date == as.Date("1994-09-18") & source_file == "TARGET93.DAT") | # Tahltan
         (lake_code == 66  & survey_date == as.Date("1994-09-13") & source_file == "TARGET93.DAT") | # Tatsamenie
         (lake_code == 66  & survey_date == as.Date("1997-06-26") & source_file == "TARGET96.DAT") | # Tatsamenie
         (lake_code == 67  & survey_date == as.Date("1994-09-11") & source_file == "TARGET93.DAT") | # Trapper   
         (lake_code == 67  & survey_date == as.Date("1994-09-11") & source_file == "TARGET94.DAT" & is.na(acoustic_survey_notes)) | # Trapper   
         (lake_code == 175 & survey_date == as.Date("1994-09-02") & source_file == "TARGET93.DAT") | # Tuya   
  
#   Continue with removal of sequential_date replicates - See "LDP Data Issues.PDF / section Sequential Survey Dates – Potential Duplicates" for details
         (lake_code == 161 & survey_date == as.Date("1986-04-15") & source_file == "TARGET86.DAT") | # Brannen
         (lake_code == 25  & survey_date == as.Date("1994-09-07") & source_file == "TARGET94.DAT") | # Eden 
         (lake_code == 1   & survey_date == as.Date("1979-12-19") & source_file == "TARGET79.DAT") | # Great Central        
         (lake_code == 1   & survey_date == as.Date("1990-10-17") & source_file == "TARGET90.DAT") | # Great Central        
         (lake_code == 1   & survey_date == as.Date("1991-03-05") & source_file == "TARGET90.DAT") | # Great Central        
         (lake_code == 1   & survey_date == as.Date("1991-03-06") & source_file == "TARGET90.DAT") | # Great Central        
         (lake_code == 1   & survey_date == as.Date("1998-12-02") & source_file == "TARGET98.DAT") | # Great Central        
         (lake_code == 1   & survey_date == as.Date("2000-11-30") & source_file == "TARGET00.DAT") | # Great Central        
         (lake_code == 1   & survey_date == as.Date("2001-12-04") & source_file == "TARGET01.DAT") | # Great Central        
         (lake_code == 1   & survey_date == as.Date("2004-01-22") & source_file == "TARGET03.DAT") | # Great Central        
         (lake_code == 3   & survey_date == as.Date("1986-09-23") & source_file == "TARGET86.DAT") | # Henderson            
         (lake_code == 3   & survey_date == as.Date("1993-02-25") & source_file == "TARGET92.DAT") | # Henderson            
         (lake_code == 3   & survey_date == as.Date("1993-07-16") & source_file == "TARGET93.DAT") | # Henderson            
         (lake_code == 3   & survey_date == as.Date("1995-06-20") & source_file == "TARGET95.DAT") | # Henderson            
         (lake_code == 3   & survey_date == as.Date("2004-03-09") & source_file == "TARGET03.DAT") | # Henderson            
         (lake_code == 3   & survey_date == as.Date("2004-11-29") & source_file == "TARGET04.DAT") | # Henderson            
         (lake_code == 3   & survey_date == as.Date("2004-11-30") & source_file == "TARGET04.DAT") | # Henderson            
         (lake_code == 3   & survey_date == as.Date("2007-08-23") & source_file == "TARGET07.DAT") | # Henderson            
         (lake_code == 50  & survey_date == as.Date("2000-11-10") & source_file == "TARGET00.DAT") | # Heydon               
         (lake_code == 6   & survey_date == as.Date("1987-04-21") & source_file == "TARGET87.DAT") | # Hobiton            
         (lake_code == 26  & survey_date == as.Date("2003-09-07") & source_file == "TARGET03.DAT") | # Ian    
         (lake_code == 41  & survey_date == as.Date("1993-10-20") & source_file == "TARGET93.DAT") | # Kennedy Main            
         (lake_code == 41  & survey_date == as.Date("1993-10-21") & source_file == "TARGET93.DAT") | # Kennedy Main         
         (lake_code == 801 & survey_date == as.Date("2007-02-19") & source_file == "TARGET06.DAT") | # Long (A)
         (lake_code == 801 & survey_date == as.Date("2007-11-07") & source_file == "TARGET07.DAT") | # Long (A)
         (lake_code == 801 & survey_date == as.Date("2008-02-22") & source_file == "TARGET07.DAT") | # Long (A)
         (lake_code == 802 & survey_date == as.Date("2007-02-19") & source_file == "TARGET06.DAT") | # Long (B)
         (lake_code == 802 & survey_date == as.Date("2007-11-07") & source_file == "TARGET07.DAT") | # Long (B)
         (lake_code == 802 & survey_date == as.Date("2008-02-22") & source_file == "TARGET07.DAT") | # Long (B)
         (lake_code == 801 & survey_date == as.Date("2007-02-19") & source_file == "TARGET06.DAT") | # Long (A)
         (lake_code == 801 & survey_date == as.Date("2007-11-07") & source_file == "TARGET07.DAT") | # Long (A)
         (lake_code == 32  & survey_date == as.Date("1986-08-15") & source_file == "TARGET86.DAT") | # Mercer  
         (lake_code == 43  & survey_date == as.Date("1991-08-30") & source_file == "TARGET91.DAT") | # Meziadin
         (lake_code == 59  & survey_date == as.Date("1986-08-21") & source_file == "TARGET86.DAT") | # Phillips
         (lake_code == 238 & survey_date == as.Date("2004-03-02") & source_file == "TARGET03.DAT") | # Osoyoos (N)
         (lake_code == 238 & survey_date == as.Date("2005-08-04") & source_file == "TARGET05.DAT") | # Osoyoos (N)
         (lake_code == 228 & survey_date == as.Date("2004-02-04") & source_file == "TARGET03.DAT") | # Owikeno (A)
         (lake_code == 228 & survey_date == as.Date("2007-02-14") & source_file == "TARGET06.DAT") | # Owikeno (A)
         (lake_code == 228 & survey_date == as.Date("2008-02-20") & source_file == "TARGET07.DAT") | # Owikeno (A)
         (lake_code == 229 & survey_date == as.Date("2004-02-04") & source_file == "TARGET03.DAT") | # Owikeno (B)
         (lake_code == 229 & survey_date == as.Date("2007-02-14") & source_file == "TARGET06.DAT") | # Owikeno (B)
         (lake_code == 229 & survey_date == as.Date("2008-02-19") & source_file == "TARGET07.DAT") | # Owikeno (B)
         (lake_code == 61  & survey_date == as.Date("2002-09-16") & source_file == "TARGET02.DAT") | # Sakinaw    
         (lake_code == 2   & survey_date == as.Date("1995-11-20") & source_file == "TARGET95.DAT") | # Sproat     
         (lake_code == 156 & survey_date == as.Date("1994-08-29") & source_file == "TARGET94.DAT") | # Skidegate NE
         (lake_code == 156 & survey_date == as.Date("1994-08-31") & source_file == "TARGET94.DAT") | # Skidegate NE
         (lake_code == 67  & survey_date == as.Date("1991-09-16") & source_file == "TARGET91.DAT") | # Trapper     
         (lake_code == 175 & survey_date == as.Date("1996-09-09") & source_file == "TARGET96.DAT") | # Tuya
         (lake_code == 69  & survey_date == as.Date("1998-08-14") & source_file == "TARGET98.DAT"))  # Yakoun
           
cat("\n Duplicate Survey Data Removed
(see LDP Data Issues.docs for details)\n")
removed_duplicate_records <- target_data_duplicates_to_remove %>%
  count(lake.y, survey_date, source_file) %>%
  arrange(lake.y)
total_row <- removed_duplicate_records %>%
  summarise(
    lake.y = "TOTAL",
    survey_date = NA,
    source_file = NA,
    n = sum(n))
frequency_table_with_total <- bind_rows(removed_duplicate_records, total_row)
print(frequency_table_with_total, n = Inf)                                      # 830 records associated with key-field duplicates removed
cat("\n")                                                                       # 5672 records in total, including seq_date dups

# Remove duplicates from merged_data_strata using source_file and line_number
target_data_duplicates_removed <- merged_data_strata %>%
  anti_join(target_data_duplicates_to_remove, by = c("source_file", "line_number"))

#   Clean up and flag data issues in data_issues field ####
merged_data_with_issues <- target_data_duplicates_removed %>%                   # was <-merged_data_strata %>%
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
    
    # set HOBITON Lk survey 880614 to JUVENILE survey_type and survey_type_code (was classed ADULT due to text in comment, but should be juv
    target_survey_code = case_when(
      lake_code == 6 & survey_date == ymd("1988-06-14") ~ 1,
      TRUE ~ target_survey_code),
    
    target_survey_type = case_when(
      lake_code == 6 & survey_date == ymd("1988-06-14") ~ "JUVENILE",
      TRUE ~ target_survey_type),
    
    
    # set JANSEN Lk survey 860213 to ADULT by adding ADULT SURVEY to survey_comments for later tranlation to survey_type and survey_type_code
    target_survey_code = case_when(
      lake_code == 108 & survey_date == ymd("1986-02-13") ~ 2,
      TRUE ~ target_survey_code),

    target_survey_type = case_when(
      lake_code == 108 & survey_date == ymd("1986-02-13") ~ "ADULT",
      TRUE ~ target_survey_type),
        
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
  tally() %>%
  arrange(lake.x)
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

# DUPLICATES analysis ####
#   Identify EXACT duplicate records (on all fields) for removal ####
target_data_exact_duplicates <- merged_tidy_data %>%             # was all_target_data %>%
  filter(duplicated(select(., -line_number, -source_file, -data_issues))) %>%  # all fields, excluding line_number and source_file
  arrange(lake, lake_code, survey_date, source_file, line_number, transect) %>%
  select( lake, lake_code, survey_date, source_file, line_number, transect, depth_code, everything()) # re-order
# View(target_data_exact_duplicates)
# Export to CSV
write_csv(target_data_exact_duplicates, paste("./output/Target_CHK_exact_duplicate_records_", date_stamp, ".csv", sep=""))
write_csv(target_data_exact_duplicates, paste("./output/Target_CHK_exact_duplicate_records_", "GENERIC.csv"     , sep="")) # save copy for import to Pivot workbook

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

#   Flag further POTENTIAL duplicates that are duplicates on key fields #### 
target_data_keyfield_duplicates <- target_data_exact_dups_removed %>%
  group_by(lake_code, survey_date, transect, depth_code) %>%  # Key fields: lake_code, survey_date, transect, depth_code
  filter(n() > 1) %>%                                         # Filter for export any records where more than one survey exists for same key fields
  mutate(key_field_replicate = "Replicate exists for this lake, date, transect and depth") %>%   # add new data issues column for keyfield replicates
  ungroup() %>%
  select(lake, lake_code, ats_year, target_survey_code, target_survey_type, survey_date, survey_year, survey_month, 
         transect, depth_code, depth_min, depth_max, targets, prop_sockeye, prop_stickleback, total_prop,
         transect_length, area,
         sounder_code, sounder_type, sounder_gain,
         data_issues, survey_comments, acoustic_survey_notes, key_field_replicate,
         source_file, line_number, sourcefile_year, source_year_err) %>%
  arrange(lake, ats_year, survey_date)

#   Export the duplicate records with line numbers but DO NOT REMOVE key field duplicates from the data until after inspection
write.csv(target_data_keyfield_duplicates, paste("./output/Target_CHK_keyfield_duplicate_surveys_", date_stamp, ".csv", sep=""), row.names = FALSE) 
write.csv(target_data_keyfield_duplicates, paste("./output/Target_CHK_keyfield_duplicate_surveys_GENERIC",      ".csv", sep=""), row.names = FALSE) # dump out a generic version that is auto-read by Excel PIVOT version

#   Repeat key-field duplicates check but do not filter, just flag the situation in key_field_replicate column 
target_data_keyfield_dups_flagged <- target_data_exact_dups_removed %>%
  group_by(lake_code, survey_date, transect, depth_code) %>%  # Key fields: lake_code, survey_date, transect, depth_code
  mutate(key_field_replicate = ifelse(n() > 1, "Replicate exists for this lake, date, transect and depth","")) %>%   # create new "data issues" column for keyfield replicates
  ungroup()

#   Flag further POTENTIAL duplicates based on sequential survey_date pairs for the same lake ####
lake_surveys_unique <- target_data_keyfield_dups_flagged %>%                    
  select(ats_year, lake, lake_code, survey_date, target_survey_code, target_survey_type, 
         sounder_code, sounder_type, source_file, 
         acoustic_survey_notes, acoustic_survey_comments = survey_comments) %>%
  distinct() %>%                                                                # get unique lake surveys from meta-data
  arrange(ats_year, lake, lake_code, survey_date)

lake_survey_sequential_pairs <- lake_surveys_unique %>%                         # identify which lake surveys follow sequentially, 
  group_by(target_survey_type, sounder_code, lake) %>%                          #  (excluding survey_type or sounder_type replicates), 
  arrange(survey_date, .by_group = TRUE) %>%                                    #  indicating possible:
  mutate(prev_date = lag(survey_date),                                          #  replicate analyses (e.g., with diff software, or revised spp proportions), or
         next_date = lead(survey_date),                                         #  replicate readings (e.g., with diff processors), or
         is_sequential = (as.numeric(survey_date - prev_date)   == 1 |          #  potential erroneous duplicates.
                          as.numeric(next_date   - survey_date) == 1)) %>%
  filter(is_sequential) %>%
  arrange(lake, sounder_code, survey_date) %>%
  mutate(pair_start = (as.numeric(survey_date - prev_date) != 1),
         sequential_survey_pair_id = cumsum(pair_start)) %>%
  mutate(sequential_survey_pair_id = ifelse(is.na(sequential_survey_pair_id), 99, sequential_survey_pair_id)) %>%
  select(-prev_date, -next_date, -is_sequential, -pair_start) %>%
  ungroup()

# Select key columns from lake_survey_sequential_pairs
lake_survey_keys <- lake_survey_sequential_pairs %>%
  select(ats_year, lake_code, survey_date, target_survey_type, sounder_code, sequential_survey_pair_id)

# Perform a semi-join to keep only matching records from lake_surveys_unique
lake_sequential_survey_data <- target_data_keyfield_dups_flagged %>%            
  semi_join(lake_survey_keys, by = c("ats_year", "lake_code", "survey_date")) %>%
  mutate(sequential_date_replicate = "Replicate or potential duplicate (sequential survey dates)") %>%
  select(ats_year, lake, lake_code, survey_date, target_survey_type, 
         sounder_type, sounder_code, 
         transect, depth_code, targets, prop_sockeye, 
         source_file, line_number, 
         survey_comments, acoustic_survey_notes, sequential_date_replicate) %>% 
  arrange(lake, survey_date, target_survey_type, sounder_code)

# export all surveys with same lake and sequential dates to csv for survey comparison via Excel Pivot tables
write_csv(lake_sequential_survey_data, paste("./output/Target_CHK_sequential_surveys_", date_stamp, ".csv", sep=""))
write.csv(lake_sequential_survey_data, paste("./output/Target_CHK_sequential_surveys__GENERIC",     ".csv", sep=""), row.names = FALSE) # dump out a generic version that is auto-read by Excel PIVOT version

# Flag sequential surveys without merging (due to possible many-to-many match-merge) and without removal
target_data_sequential_flagged <- target_data_keyfield_dups_flagged %>%
  rowwise() %>%
  mutate(sequential_date_replicate = if_else(
    any(lake_code == lake_survey_sequential_pairs$lake_code &
          survey_date == lake_survey_sequential_pairs$survey_date),
    "Replicate or potential duplicate (sequential survey dates)",
    NA_character_
  )) %>%
  ungroup()

# MISSING/INVALID values analysis ####

cat("\nMissing Values (NAs) Summary\n")
cat("  Number of records should be zero for all index variables (i.e., lake, date, depth, transect) and target counts (targets), 
  but may not be 0 for proportions and meta-data variables (data_issues, key_field_replicates).\n")
NA_missing_summary <- sapply(target_data_sequential_flagged, function(x) sum(is.na(x)))
print(NA_missing_summary)
cat("\n")

# cat("\nTally Frequency of data_issues by Type\n")                             # moved to final output, below
# summary_table <- target_data_sequential_flagged %>%
#   filter(data_issues != "") %>%
#   group_by(data_issues) %>%
#   summarise(count = n(), .groups = "drop") %>%
#   arrange(data_issues, desc(count)) %>%
#   bind_rows(
#     summarise(., data_issues = "TOTAL RECORDS WITH DATA ISSUES ----------------------->", 
#               count = sum(count)))
# print(summary_table, n = Inf)
# cat("\n")

#   Checking for NAs in depth code - especially if TARGETS > 0 
cat("\nMissing Depth Codes when Targets > 0\n")
missing_depth_code <- target_data_sequential_flagged %>%                        # was <-merged_data %>%
  filter(is.na(depth_code) & targets > 0)                 # Initially numerous records with missing depths but targets > 0  # FIXED! {hs 250909}
print(missing_depth_code)
cat("\n")

#   Invalid date check ####
cat("\nInvalid Date Check")
target_date_err_chk <- target_data_sequential_flagged %>%
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

#   Metadata variable range checks ####
cat("\nNumeric Metadata Variable Check: Lake, Dates, Sounder values...")
#   Select numeric columns and summarize missing, min, and max
target_metadata <- target_data_sequential_flagged %>%
  select(ats_year, lake, survey_date, sounder_type, where(is.numeric)) %>%
  select(-line_number, -targets, -prop_sockeye, -prop_stickleback, -total_prop, -depth_code, -depth_min, -depth_max, -transect, -transect_length, -area) %>%
  mutate(survey_month = as.numeric(as.character(target_data_sequential_flagged$survey_month))) %>%
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
merged_data_final_chk <- target_data_sequential_flagged %>%             # flag but do not change sounder_code and sounder_gain issues, errors
  mutate(sounder_issues = "" ) %>%
  mutate(sounder_issues = ifelse(sounder_code == 1 & !str_detect(sounder_type, "FURUNO") |
                                 sounder_code == 2 & !str_detect(sounder_type, "SIMRAD") |
                                !sounder_code %in% c(1, 2) | is.na(sounder_code), 
                                 str_c("Sounder Code/Type discrepancy;"), sounder_issues)) %>%
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

#   Data variable range checks ####
#   Main numeric data issues (flagged only, not changed) include:
#      total_prop(ortion) sometimes > 1 (and as shown elsewhere, sometimes < 1)
#      targets < 0

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
write_csv(range_issues, paste("./output/Target_CHK_targets_issues_", date_stamp, ".csv", sep=""))

# FINAL DATA OUTPUT (Post-processing) ####
# Export cleaned up acoustic survey data with target data records 
acoustic_target_final_data <- merged_data_final_chk %>% 
  mutate(data_issues = str_c(data_issues, " ", sounder_issues)) %>%  # amalgamate all data issues into the data_issues field
  rename(depth_min_m = depth_min,
         depth_max_m = depth_max,
         area_ha = area,
         transect_length_m = transect_length) %>% 
  select(ats_year, lake_code, lake, survey_date, target_survey_code, target_survey_type, survey_year, survey_month, transect, 
         depth_code, depth_min_m, depth_max_m, targets, prop_sockeye, prop_stickleback, prop_total = total_prop, 
         transect_length_m, area_ha, 
         sounder_code, sounder_type, sounder_gain, sounder_issues,
         acoustic_survey_notes, acoustic_survey_comments = survey_comments, 
         data_issues, key_field_replicate, sequential_date_replicate, 
         acoustic_source_file = source_file, line_number, everything(), -sourcefile_year, -source_year_err) %>%
  arrange(ats_year, lake, survey_date, transect, depth_code)

# Segregate adult and juvenile type surveys
adult_target_data <- acoustic_target_final_data %>%
  filter(target_survey_type == "ADULT")

juvenile_target_data <- acoustic_target_final_data %>%
  filter(target_survey_type == "JUVENILE")

write_csv(acoustic_target_final_data, paste("./output/Target_OUTPUT_data_FINAL_", ats_year_span, date_stamp, ".csv", sep=""))
write_csv(acoustic_target_final_data, paste("./output/Target_OUTPUT_data_FINAL_", ats_year_span, "GENERIC.csv",      sep="")) # export copy for PIVOT workbook

# write_csv(adult_target_data, paste("./output/Target_OUTPUT_ATS_Adult_", date_stamp, ".csv", sep=""))
# write_csv(juvenile_target_data, paste("./output/Target_OUTPUT_ATS_Juvenile", date_stamp, ".csv", sep=""))

cat("\nTally Frequency of data_issues by Type\n")
summary_table <- acoustic_target_final_data %>%
  mutate(data_issues = str_trim(data_issues)) %>%  # Remove leading/trailing whitespace
  filter(data_issues != "" & data_issues != " ") %>%
  group_by(data_issues) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(data_issues, desc(count)) %>%
  bind_rows(
    summarise(., data_issues = "TOTAL RECORDS WITH DATA ISSUES ----------------------->", 
              count = sum(count)))
print(summary_table, n = Inf)
cat("\n")

#   Export final data inventory (unique surveys)
acoustic_target_final_inventory <- merged_data_final_chk %>%
  select(ats_year, lake, lake_code, survey_date, 
         target_survey_code, target_survey_type, 
         sounder_code, sounder_type, sounder_gain, sounder_issues, 
         key_field_replicate, sequential_date_replicate,
         acoustic_survey_notes, acoustic_survey_comments = survey_comments, source_file) %>%
  distinct() %>%
  arrange(ats_year, lake, lake_code, survey_date)

write_csv(acoustic_target_final_inventory, paste("./output/Target_OUTPUT_data_INVENTORY_", ats_year_span, date_stamp, ".csv", sep=""))
write_csv(acoustic_target_final_inventory, paste("./output/Target_OUTPUT_data_INVENTORY_", ats_year_span, "GENERIC.csv",      sep="")) # export copy for PIVOT workbook

cat("\nTally Frequency of Surveys that are potential duplicates/replicates\n")
summary_table <- acoustic_target_final_inventory %>%
  mutate(
    key_field_replicate = ifelse(is.na(key_field_replicate), "", key_field_replicate),
    sequential_date_replicate = ifelse(is.na(sequential_date_replicate), "", sequential_date_replicate),
    replicate_issue = paste(str_trim(key_field_replicate), str_trim(sequential_date_replicate))) %>%
  mutate(replicate_issue = str_trim(replicate_issue)) %>%
  filter(replicate_issue != "") %>%
  group_by(replicate_issue) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(replicate_issue, desc(count)) %>%
  bind_rows(
    summarise(., replicate_issue = "TOTAL SURVEYS WITH POTENTIAL DUPLICATES/REPLICATES ----------------------->", 
              count = sum(count)))
print(summary_table, n = Inf)
cat("\n")

# COMPARE RAW and FINAL Acoustic Survey Dates ####
#
#   Match-merge RAW and FINAL acoustic survey metadata 
#   (i.e., by lake and survey_date) from LDP program
#   Acoustic Target Data Cleanup.R to ensure nothing lost in process

#   Select relevant columns and get distinct combinations
raw_target_data_reduced <- all_target_data %>%   # <- this is the RAW target data
  select(lake_code, lake, survey_date, acoustic_survey_notes, source_file) %>%
  distinct()

final_target_data_reduced <- merged_data_final_chk %>%  # <- this is the CLEANED target data
  select(lake_code, lake, survey_date, acoustic_survey_notes, source_file) %>%
  distinct()

#   Add source indicators
raw_target_data_reduced <- raw_target_data_reduced %>%
  mutate(source = "A_RAW_target_data")

final_target_data_reduced <- final_target_data_reduced %>%
  mutate(source = "B_FINAL_target_data")

#   Full join to combine both datasets
combined1 <- full_join(raw_target_data_reduced, final_target_data_reduced,
                       by = c("lake_code", "survey_date"),
                       suffix = c("_all", "_merged"))
#   Warning message:
#   In full_join(raw_target_data_reduced, final_target_data_reduced,  :
#   Detected an unexpected many-to-many relationship between `x` and `y`.
#     ℹ Row 132 of `x` matches multiple rows in `y`.
#     ℹ Row 991 of `y` matches multiple rows in `x`.

#   Determine final source label
combined2 <- combined1 %>%
  mutate(source = case_when(
    !is.na(source_all) & !is.na(source_merged) ~ "C_In_Both_RAW_and_FINAL_data",
    !is.na(source_all)    ~ "A_RAW_target_data_only",
    !is.na(source_merged) ~ "B_FINAL_target_data_only"
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

write_csv(combined_raw_and_final, paste("./output/Target_CHK_RAW_vs_CLEAN_surveys_", date_stamp, ".csv", sep=""))
# VISUALIZE the frequency of acoustic surveys by Lake and ATS Year ####
#   Set the lake and lake_codes to be the same for things like Heydon Lk and Heydon_2005...
acoustic_target_final_inventory_tidy <- acoustic_target_final_inventory %>%
  mutate(lake = str_replace(lake, "_2005", " Lk"),          # consolidate lake names
         lake = str_replace(lake, "_2006", " Lk"),          # where they are suffixed with Year
         lake = str_replace(lake, "_2007", " Lk"),          
         lake = gsub("\\s*\\(.*\\)", "", lake),             # and remove (A), (B)...(D), (N), (S) 
         lake = gsub("\\s+(ne|nw|se|sw)$", "", lake))       # and remove ne, nw, se, sw 

# Get unique lakes sorted alphabetically
lake_groups <- acoustic_target_final_inventory_tidy %>%  
  distinct(lake) %>%
  arrange(lake) %>%
  mutate(lake_group = LETTERS[ceiling(row_number() / 9)])

# Join lake group back to full dataset
acoustic_data_visualize <- acoustic_target_final_inventory_tidy %>%
  mutate(decade = paste0((ats_year %/% 10) * 10, "s")) %>%
  left_join(lake_groups, by = "lake") %>%
  select(lake_group, decade, ats_year, lake, lake_code, survey_date) %>%
  arrange(lake_group, lake, decade, ats_year)

# Prepare data for visualization: surveys per lake and year
record_counts <- acoustic_data_visualize %>%
  group_by(lake_group, decade, lake, ats_year) %>%
  summarise(record_count = n(), .groups = "drop")

# Generate one plot per lake_group
unique_groups <- sort(unique(record_counts$lake_group))

# Define the plot file path
plotfile <- paste0("./figures/Acoustic_Survey_Freq_by_Lake_Year_", ats_year_span, date_stamp, ".pdf")

# Check if the file exists
if (file.exists(plotfile)) {
  # Check if the file is currently open in R
  if (any(grepl("pdf", names(dev.list())))) {
    dev.off()  # Close the PDF device
  }
  
  # Try to remove the file
  tryCatch({
    file.remove(plotfile)
  }, warning = function(w) {
    message("Warning: Could not delete existing file. It may be open or locked.")
  }, error = function(e) {
    message("Error: Unable to delete file. Check permissions or if it's open elsewhere.")
  })
}

# Open a new PDF device
pdf(plotfile, width = 11, height = 8.5) 

for (group in unique_groups) {
  group_data <- record_counts %>% filter(lake_group == group)
  
  # Calculate max record_count for this group
  max_count <- max(group_data$record_count, na.rm = TRUE)
  
  p <- group_data %>%
    ggplot(aes(x = ats_year, y = record_count)) +
    geom_bar(stat = "identity", fill = "steelblue", color = "white") +
    facet_wrap(~ lake, scales = "fixed", ncol = 3) +
    scale_y_continuous(limits = c(0, max_count)) +
    scale_x_continuous(breaks = seq(1977, 2007, by = 5), limits = c(1975, 2010)) +
    labs(title = "Frequency of Acoustic Surveys by Lake and ATS-year",
         subtitle = paste0("Source: Acoustic Target Data Cleanup.R  ", "Date: ", format(as.Date(date_stamp, format = "%y%m%d"), "%d-%b-%Y"), sep=""), # date-stamp the plots
         x = "", y = "") +
    theme_minimal() +
    theme(plot.title    = element_text(color = "steelblue", size = 14, face = "bold", hjust = 0),
          plot.subtitle = element_text(color = "darkgray",  size = 8,  face = "bold", hjust = 0),
          strip.text    = element_text(size = 12),
          axis.text.x   = element_text(angle = 45, hjust = 1))
  
  print(p)}  # Or use ggsave() to save each plot

# Close the PDF device
close <- dev.off()
# close

# FINISH ####
#   Calculate execution time and finish up
end_time <- Sys.time()
execution_time <- as.numeric(difftime(end_time, start_time, units = "mins"))
cat("\nExecution time:", round(execution_time, 2), "minutes\n")
make_a_sound("coin", .25)
