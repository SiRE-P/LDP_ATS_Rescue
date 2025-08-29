library(tidyverse)
library(tools)
library(lubridate)
library(stringr)

parse_target_dat <- function(filepath) {
  raw_lines <- read_lines(filepath)
  raw_lines <- raw_lines[trimws(raw_lines) != ""]  ### remove blank lines
  
  ### find the start of each data block
  header_indices <- which(str_detect(raw_lines, "depth / transect / targets"))
  all_data <- list()  ### to collect parsed chunks
  
  for (i in seq_along(header_indices)) {
    header_index <- header_indices[i]
    
    ### define metadata block (everything above header)
    metadata_block <- raw_lines[max(1, header_index - 10):(header_index - 1)]
    
    ### extract metadata
    lake_code <- metadata_block[str_detect(metadata_block, "lake code")] %>%
      str_extract("\\d+(?=\\s*: lake code)") %>% 
      as.numeric()
    
    lake <- metadata_block[str_detect(metadata_block, "lake code", negate = TRUE)] %>%
      .[str_detect(., "^[A-Z]")] %>% str_trim() %>% .[1]
    
    date_str <- metadata_block[str_detect(metadata_block, ": date")] %>%
      str_extract("\\d{6}") %>% .[1]
    
    survey_date <- ymd(date_str)
    
    sounder <- metadata_block[str_detect(metadata_block, "FURUNO|SIMRAD")] %>%
      str_extract("FURUNO[^:]*|SIMRAD[^:]*") %>%
      str_trim() %>% .[1]
    
    gain <- metadata_block[str_detect(metadata_block, "Gain")] %>%
      str_extract("\\d+") %>% as.numeric() %>% .[1]
    
    ### capture acoustic survey notes
    acoustic_survey_notes <- metadata_block %>%
      discard(~ str_detect(.x, "lake code|: date|FURUNO|SIMRAD|Gain|^[A-Z]")) %>%
      str_trim() %>%
      paste(collapse = " ") %>%
      na_if("")
    
    ### get data lines for this chunk
    data_start <- header_index + 1
    data_end <- if (i < length(header_indices)) header_indices[i + 1] - 1 else length(raw_lines)
    data_lines <- raw_lines[data_start:data_end]
    
    ### parse each data line into a tibble
    parsed_data <- data_lines %>%
      str_trim() %>%
      discard(~ .x == "") %>%
      str_split("\\s+") %>%
      keep(~ length(.x) >= 5) %>%
      map(~ .x[1:5]) %>%
      map_dfr(~ tibble(
        depth = .x[1] %>% str_extract("^\\d+-\\d+"),
        transect = as.numeric(.x[2]),
        targets = as.numeric(.x[3]),
        pct_sockeye = as.numeric(.x[4]),
        pct_stickleback = as.numeric(.x[5])
      )) %>%
      mutate(
        lake = lake,
        lake_code = lake_code,
        survey_date = survey_date,
        sounder_type = sounder,
        gain = gain,
        acoustic_survey_notes = acoustic_survey_notes
      )
    
    all_data[[i]] <- parsed_data
  }
  
  ### combine all chunks
  final_data <- bind_rows(all_data)
  return(final_data)
}


### list all .dat files in the working directory
dat_files <- list.files(
  path = "data",              # look in the data/ folder
  pattern = "\\.dat$",        # only .dat files
  ignore.case = TRUE,
  full.names = TRUE           # include full path so read_lines() works
)

### parse all files and combine into one tibble
all_target_data <- dat_files %>%
  set_names(~ tools::file_path_sans_ext(basename(.x))) %>%   
  map_dfr(parse_target_dat, .id = "source_file")      

data <- all_target_data %>%
  
  ### separating depth into min and max
  dplyr::mutate(depth = str_trim(depth)) %>%
  separate(depth, into = c("depth_min", "depth_max"), sep = "\\s*[-–]\\s*", convert = TRUE) %>% 
  
  # moving the extra information from metablock into a comments column 
  separate(lake,
           into = c("lake_name", "survey_comments"),
           sep = "\\s{2,}",
           extra = "merge",
           fill = "right") %>% 
  
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
    depth_min == 90 & depth_max == 100 ~ 13
  )) %>%
  
  # separating sounder type and sounder code
  extract(sounder_type, into = c("sounder_type", "sounder_code"), regex = "^(\\S+)\\s+(.*)$") %>% 
  rename(sounder_gain = gain) %>% 
  
  # renaming columns 
  rename(lake = lake_name) %>% 
  
  #rearranging column order
  select(
    lake_code,
    lake,
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
    sounder_gain,
    sounder_type,
    sounder_code,
    sounder_gain,
    acoustic_survey_notes,
    survey_comments,
    source_file,
    everything()
  ) %>% 

##### Addition of ats year
  mutate(
    # survey_date = as.Date(survey_date),
    ats_year = if_else(month(survey_date) >= 4,
                       year(survey_date),
                       year(survey_date) - 1),
    .before = survey_date) %>% 
  
  # I think this is to drop the blank rows in between the collections
  drop_na()

  # add in columns from lake_strata (area, length)
lake_strata <- read.csv("./data/lake_strata_lengths.csv") #input the reference file 

merged_data <- data %>%
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

merged_data <- merged_data %>%
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
      TRUE ~ data_issues
    )
  ) %>% 
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
               "JANSEN", "MUCHALAT", "PORT JOHN", "GREAT CENTRAL", 
               "Alistair Lk"),
  lake_new = c("Great Central Lk", "Awun Lk", "Henderson Lk", "Henderson Lk", 
               "Eden Lk", "Bonilla Lk", "Devon Lk", "Hobiton Lk", "Kitlope Lk", 
               "Lowe Lk", "Long Lk (A)", "Long Lk (B)", "Mercer Lk", "Muriel Lk", 
               "Muriel Lk", "Nimpkish Lk", "Woss Lk", "Alistair Lk", "Curtis Lk", 
               "Fred Wright Lk", "Ian Lk", "Yakoun Lk", "Cheewhat Lk", "Sproat Lk", 
               "Jansen Lk", "Muchalat Lk", "Port John Lk", "Great Central Lk", "Alastair Lk")
)

# Join and replace
merged_data <- merged_data %>%
  left_join(lake_lookup, by = c("lake.x" = "lake_old")) %>%
  mutate(lake.x = coalesce(lake_new, lake.x)) %>%
  select(-lake_new)

merged_data$lake.x %>% unique() %>% sort(.)

n_distinct(merged_data$lake.x)
n_distinct(merged_data$lake_code)

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


#### NEED TO DEAL WITH WEATHER STILL 
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
# merged_data %>% 
#   filter(is.na(depth_code)) # GREAT!

# From Howard's checks code: 
# Identify further duplicates on key fields #### 
merged_data <- merged_data %>%
  group_by(lake_code, survey_date, transect, depth_code) %>%
  mutate(
    key_field_replicate = if_else(
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

