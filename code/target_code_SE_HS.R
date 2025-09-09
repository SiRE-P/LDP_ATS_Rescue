# Libraries ####

library(dplyr)
library(lubridate)
library(purrr)
library(stringr)
library(tidyverse)
library(tibble)
library(tictoc)       # get elapsed time 
library(tools)

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
    walk(estimated_lines, ~ message(sprintf("NOTE: form feed character encountered in line %d", .x)))  }
  
  line_numbers <- seq_along(raw_lines)  # actual line numbers in the file
 
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
    
    sounder <- metadata_block[str_detect(metadata_block, "FURUNO|SIMRAD")] %>%
      str_extract("FURUNO[^:]*|SIMRAD[^:]*") %>%
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

### list all .dat files in the working directory
dat_files <- list.files(
  path = "data",              # look in the data/ folder
  pattern = "\\.dat$",        # only .dat files
  ignore.case = TRUE,
  full.names = TRUE)          # include full path so read_lines() works

print(dat_files)              # list the import DAT files (TARGET*.DAT)

### parse all files and combine into one tibble
tic("Compilation time: ")
all_target_data <- dat_files %>%
  set_names(~ tools::file_path_sans_ext(basename(.x))) %>%   
  map_dfr(parse_target_dat_tracer, .id = "source_file") %>%     # modified function and call to add source file and line no's for traceability (HS 25-09-08)
  filter(!if_all(c(transect, depth, targets), is.na))           # this drops all records missing in three key index variables, which seems to happen due to blank lines between input data blocks (HS 250908)
toc()

write_csv(all_target_data, "./output/all_target_data_SE_HS.csv")

# Error-Check target data ####

# Identify exact duplicate records #
target_data_exact_duplicates <- all_target_data %>%
  filter(duplicated(select(., -line_number))) #  excluding line_number

data <- all_target_data %>%
  
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
  
  # Assigning target_survey_type -- ADULT vs JUVENILE (default) -- based on survey_comments
  
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
                                  "NO COMMENTS", 
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
    depth_min == 90 & depth_max == 100 ~ 13)) %>%
  
  # separating sounder type and sounder code
  extract(sounder_type, into = c("sounder_type", "sounder_code"), regex = "^(\\S+)\\s+(.*)$") %>% 
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
View(target_data_keyfield_duplicates)
# Export to CSV
write.csv(target_data_keyfield_duplicates, "/output/target_data_keyfield_duplicates.csv", row.names = FALSE) 

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
    data_issues = ""
  ) %>%
  mutate(
    total_prop = prop_stickleback + prop_sockeye,
    
    # prop_sockeye / prop_stickleback zeros flagged as NA
    data_issues = case_when(
      is.na(prop_sockeye) & is.na(prop_stickleback) ~
        str_c(data_issues, "prop_sockeye and prop_stickleback = zero, set to NA"),
      TRUE ~ data_issues
    ),
    
    # proportion check 
    data_issues = case_when(
      total_prop > 1.01 ~ str_c(data_issues, "; incorrect proportion of fish"),
      total_prop < 0.99 ~ str_c(data_issues, "; incorrect proportion of fish"),
      TRUE ~ data_issues
    ),
    
    # Year vs ats_year 
    data_issues = case_when(
      (survey_year == ats_year & survey_month > 4) |
        (survey_year == ats_year + 1 & survey_month < 5) ~ data_issues,
      TRUE ~ str_c(data_issues, "; Survey not in ATS Year")
    ),
    
    # SKAHA Lake fix for invalid date (00/09/31)
    fix_skaha_date = lake_code == 241 & source_file == "TARGET00" & is.na(survey_date),
    survey_date = case_when(
      fix_skaha_date ~ ymd("2000-10-01"),
      TRUE ~ survey_date),
    survey_year = year(survey_date),
    survey_month = month(survey_date),
    ats_year = assign_ats_year(survey_date),
    
    data_issues = case_when(
      fix_skaha_date ~ str_c(data_issues, "; missing or invalid survey_date (31-Sep-00), assigned (01-Oct-00) from survey comment info"),
      TRUE ~ data_issues
    ),
    
    # Megin Lake fix
    survey_date = case_when(
      lake_code == 118 & survey_year == 1996 & survey_month == 3 ~ ymd("1996-03-20"),
      TRUE ~ survey_date
    ),
    data_issues = case_when(
      lake_code == 118 & survey_year == 1996 & survey_month == 3 ~
        str_c(data_issues, "; missing or invalid survey_date, assigned from survey comment info"),
      TRUE ~ data_issues
    ),
    
  # Muriel Lake fix
  survey_date = case_when(
    lake_code == 44 & survey_year == 1996 & survey_month == 3 ~ ymd("1996-03-22"),
    TRUE ~ survey_date
  ),
  data_issues = case_when(
    lake_code == 44 & survey_year == 1996 & survey_month == 3 ~
      str_c(data_issues, "; missing or invalid survey_date, assigned from survey comment info"),
    TRUE ~ data_issues
  ),

  # Tatsemenie fix
  ats_year = case_when(
    lake_code == 66 & survey_date == ymd("1992-08-01") ~ 1992L,
    TRUE ~ ats_year
  ),
  data_issues = case_when(
    lake_code == 66 & survey_date == ymd("1992-08-01") ~
      str_c(data_issues, "; data erroneously stored in TARGET95.DAT, ats_year updated from survey_date and processed_date"),
    TRUE ~ data_issues
  )
  ) %>%
  # Owikeno removals
  filter(!(lake_code == 228 & survey_date == ymd("2007-02-15"))) %>%
  filter(!(lake_code == 229 & survey_date %in% ymd(c("2004-02-04", "2007-02-15")))) %>%
  mutate(
    data_issues = case_when(
      lake_code == 228 & survey_date == ymd("2007-02-14") ~
        str_c(data_issues, "; duplicate data dated 070215 deleted"),
      lake_code == 229 & survey_date == ymd("2004-02-14") ~
        str_c(data_issues, "; this was a duplicate, other copy deleted; probable true date Feb 14, 2007, from acoustic_survey_notes, data found in TARGET06.DAT; unknown why the target numbers are different than for date 070214"),
      lake_code == 229 & survey_date == ymd("2007-02-14") ~
        str_c(data_issues, "; duplicate data dated 070215 deleted"),
      TRUE ~ data_issues)) %>% 
  select(-total_prop) 

# finding and fixing some lake name issues 
merged_data %>% 
  select(lake.x, lake.y) %>% 
  filter(lake.x != lake.y) %>% 
  unique() %>% 
  arrange(lake.x) %>% 
  print(n = Inf)

# Reference table of old and new names
lake_lookup <- tibble::tibble(
  lake_old = c("GCL", "AWUN", "HENDERSON", "Henderson LK", "EDEN", 
               "BONILLA", "DEVON", "HOBITON", "KITLOPE", "LOWE", "LongA", "LongB", 
               "MERCER", "MURIEL", "MURIEL LAKE", "NIMPKISH", "WOSS", "ALISTAIR", 
               "CURTIS", "FRED WRIGHT", "IAN", "YAKOUN", "CHEEWHAT", "SPROAT", 
               "JANSEN", "MUCHALAT", "PORT JOHN", "GREAT CENTRAL", "Alistair Lk"),
  lake_new = c("Great Central Lk", "Awun Lk", "Henderson Lk", "Henderson Lk", 
               "Eden Lk", "Bonilla Lk", "Devon Lk", "Hobiton Lk", "Kitlope Lk", 
               "Lowe Lk", "Long Lk (A)", "Long Lk (B)", "Mercer Lk", "Muriel Lk", 
               "Muriel Lk", "Nimpkish Lk", "Woss Lk", "Alastair Lk", "Curtis Lk", 
               "Fred Wright Lk", "Ian Lk", "Yakoun Lk", "Cheewhat Lk", "Sproat Lk", 
               "Jansen Lk", "Muchalat Lk", "Port John Lk", "Great Central Lk", "Alastair Lk"))

# Join and replace
merged_data <- merged_data %>%
  left_join(lake_lookup, by = c("lake.x" = "lake_old")) %>%
  mutate(lake.x = coalesce(lake_new, lake.x)) %>%
  select(-lake_new)

# merged_data$lake.x %>% unique() %>% sort(.)
# n_distinct(merged_data$lake_code)

merged_data %>% 
  group_by(lake_code, lake.x) %>% 
  tally() %>% 
  print(n = Inf)

# Are these the same??? If so, change lake names so they are 
# same lake codes
# KCA and Kennedy L (Clay) 
# KMA and Kennedy L (Main)

# different lake codes
# Heydon_2005 and Heydon Lk
# Nahwitti_2005 and Nahwitti Lk
# Osoyoos_2005 and ?
# Phillips_2005 and Phillips Lk
# Quatse_2005 and Quatse Lk
# Skaha_2007 and Skaha Lk

merged_data <- merged_data %>% 
  select(-lake.y) %>% 
  rename(lake = lake.x) 


#### NEED TO DEAL WITH WEATHER STILL ##  No need to extract weather info as long as comment line is captured in acoustic_survey_notes (which it is)
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
  filter(is.na(depth_code)) # GREAT!  # ? Not so sure: quite a few records with missing depths but targets > 0

# From Howard's checks code: 
# Identify further duplicates on key fields #### 
merged_data <- merged_data %>%
  group_by(lake_code, survey_date, transect, depth_code) %>%
  mutate(
    key_field_replicate = if_else(                                              # rename this field to data_issues2_key_field_replicate
      n() > 1,
      "Replicate exists for this lake, date, transect and depth",
      NA_character_
    )
  ) %>%
  ungroup()

# Categorical consistency checks ####
# Count unique lake names per lake_code
lake_check <- merged_data %>%
  group_by(lake_code) %>%
  summarize(
    n_lakes = n_distinct(lake),
    lakes = paste(unique(lake), collapse = ", ")
  ) %>%
  ungroup()

# Show only lake_codes that have more than one name
lake_check %>% filter(n_lakes > 1)

# Check uniqueness of sounder_code → sounder_type
sounder_check <- merged_data %>%
  group_by(sounder_code) %>%
  summarize(
    n_types = n_distinct(sounder_type),
    types = paste(unique(sounder_type), collapse = ", ")
  ) %>%
  ungroup()

# Show only codes with more than one type
sounder_check %>% filter(n_types > 1)

NA_missing_summary <- sapply(merged_data, function(x) sum(is.na(x)))
print(NA_missing_summary)

# Check for invalid dates ####
target_date_err_chk <- merged_data %>%
  mutate(
    parsed_date = ymd(survey_date, quiet = TRUE),
    invalid_date_flag = is.na(parsed_date),                  # Flag invalid dates
    year_mismatch_flag = year(parsed_date) != survey_year,
    future_date_flag = parsed_date > Sys.Date(),   
    line_number = row_number()) %>%
  
  filter(invalid_date_flag | year_mismatch_flag | future_date_flag)

# Range checks ####
range_issues <- merged_data %>%
  filter(depth_min > depth_max | targets < 0 | transect_length < 0 | area < 0 | sounder_gain <= 0) 
print(range_issues) # negative targets 

# Output cleaned up data ####
final_data <- merged_data %>% 
  rename(depth_min_m = depth_min,
         depth_max_m = depth_max,
         area_ha = area,
         transect_length_m = transect_length) %>% 
  select(source_file, lake_code, lake, sounder_code, sounder_type, sounder_gain, survey_date, survey_year, survey_month, ats_year, depth_code, depth_min_m, 
         depth_max_m, transect, transect_length_m, area_ha, targets, prop_stickleback, prop_sockeye,
         data_issues, key_field_replicate, acoustic_survey_notes, survey_comments, everything())

write_csv(final_data, "./output/target_clean_SE_HS.csv")

