############################################################################
####### Full code in order to process the .dat TARGET files ################
############################################################################

###### PART I: converting .DAT target file into .CSV

library(tidyverse)
library(lubridate)

parse_target_dat <- function(filepath) {
  
  ### Read all lines from the DAT file
  raw_lines <- read_lines(filepath)
  
  ### Remove blank lines from the DAT file
  raw_lines <- raw_lines[trimws(raw_lines) != ""]
  
  ### Locate the start of each data block (identified by the header row)
  header_indices <- which(str_detect(raw_lines, "depth / transect / targets"))
  
  ### Container to hold parsed data from all blocks
  all_data <- list()
  
  ### Loop through each data block
  for (i in seq_along(header_indices)) {
    header_index <- header_indices[i]
    
    ### Grab metadata lines just above the header (10 lines before)
    metadata_block <- raw_lines[max(1, header_index - 10):(header_index - 1)]
    
    ######## Extracting the metadata ########
    
    ### Extract the lake code
    lake_code <- metadata_block[str_detect(metadata_block, "lake code")] %>%
      str_extract("\\d+(?=\\s*: lake code)") %>% 
      as.numeric()
    
    ### Extract lake name
    lake <- metadata_block[str_detect(metadata_block, "lake code", negate = TRUE)] %>%
      .[str_detect(., "^[A-Z]")] %>% str_trim() %>% .[1]
    
    ### Extract the date
    date_str <- metadata_block[str_detect(metadata_block, ": date")] %>%
      str_extract("\\d{6}") %>% .[1]
    
    ### Convert to proper date
    survey_date <- ymd(date_str)
    
    ### Extract sounder type
    sounder <- metadata_block[str_detect(metadata_block, "FURUNO|SIMRAD")] %>%
      str_extract("FURUNO[^:]*|SIMRAD[^:]*") %>%
      str_trim() %>% .[1]
    
    ### Extract gain 
    gain <- metadata_block[str_detect(metadata_block, "Gain")] %>%
      str_extract("\\d+") %>% as.numeric() %>% .[1]
    
    ######## Identifying the data lines for this block ########
    
    data_start <- header_index + 1  ### starting right after the header
    data_end <- if (i < length(header_indices)) header_indices[i + 1] - 1 else length(raw_lines)
    data_lines <- raw_lines[data_start:data_end]
    
    ######## Capture acoustic survey note ########
    
    ### Take the last non-empty line of the block
    note_line <- tail(data_lines[str_trim(data_lines) != ""], 1)
    
    ### If the last line is NOT a proper data line, treat it as NA
    note_is_extra <- !str_detect(note_line, "^\\d+-\\d+\\s+\\d+\\s+\\d+")
    acoustic_survey_notes <- if (note_is_extra) note_line else NA_character_
    
    ### If it’s a note, remove it so it doesn’t get parsed as data
    if (note_is_extra) {
      data_lines <- head(data_lines, -1)
    }
    
    ######## Parse actual data lines ########
    
    parsed_data <- data_lines %>%
      str_trim() %>%                        #removing extra white space
      discard(~ .x == "") %>%               #dropping empty lines
      str_split("\\s+") %>%                 #splitting by white space
      keep(~ length(.x) >= 5) %>%           #keep only lines with 5 or more fields
      map(~ .x[1:5]) %>%                    #take first 5 fields
      map_dfr(~ tibble(
        depth = .x[1] %>% str_extract("^\\d+-\\d+"), #depth range
        transect = as.numeric(.x[2]),                #transect number
        targets = as.numeric(.x[3]),                 #number of targets
        pct_sockeye = as.numeric(.x[4]),             #prop sockeye
        pct_stickleback = as.numeric(.x[5])          # prop stickleback
      )) %>%
      mutate(
        lake = lake,
        lake_code = lake_code,
        survey_date = survey_date,
        sounder_type = sounder,
        gain = gain,
        acoustic_survey_notes = acoustic_note,  #add note (or NA)
        source_file = basename(filepath)        #add source file name, to know what file the data came from
      )
    
    ### Save parsed block into the results list
    all_data[[i]] <- parsed_data
  }
  
  ### Combine all blocks into a single tibble and return
  bind_rows(all_data)
}


######## Run the function on a .DAT file ########

file_path <- "TARGET01.DAT"
target_data <- parse_target_dat(file_path)

### Inspect the results
print(target_data)

### Save to CSV
write.csv(target_data, "TARGET01_clean_V1.csv", row.names = FALSE)


############################################################################
####### To loop over multiple files use the following: #####################
############################################################################

### Set the directory with .DAT files you are interested in
data_dir <- "target_data_inprogress"

### List all files ending with .DAT
dat_files <- list.files(path = data_dir, pattern = "\\.DAT$", full.names = TRUE, ignore.case = TRUE)

### Parse each file, add source_file column, and combine into one tibble
all_targets <- dat_files %>%
  map_df(~ parse_target_dat(.x))

### Inspect the combined data
print(all_targets)

### Save the combined data set
write_csv(all_targets, "all_targets_clean_combined.csv", row.names = FALSE)

##################################################################
#### PART II: editing of the V1 of the converted .DAT file, this code includes, 
#### minor editing of the columns, renaming and addition of other data like the depth

library(dplyr)
library(lubridate)
library(tidyverse)

data <- read_csv("TARGET01_clean_V1.csv") #input the file you are interested in editing

### separate the depth_stratum into two different columns, depth min (meters) and depth max
depth_separate <- data %>%
  mutate(depth = str_trim(depth)) %>%
  separate(depth, into = c("depth_min_m", "depth_max_m"), sep = "\\s*[-–]\\s*", convert = TRUE)

### separating and adding survey_date and survey_type

data <- depth_separate %>%
  separate(lake, into = c("lake_name", "survey_comments"), sep = "\\s{2,}", extra = "merge") %>%
  mutate(survey_date = ymd(survey_date),
         year = year(survey_date),
         survey_comments = ifelse(is.na(survey_comments) | trimws(survey_comments) == "", 
                                  "NO COMMENTS", 
                                  survey_comments))


### renaming sockeye/stickleback columns
data <- data %>%
  rename(prop_sockeye = pct_sockeye)

data <- data %>%
  rename(prop_stickleback = pct_stickleback)

### adding program notes and populating with my info 

data <- data %>%
  mutate(program_notes = "YS - Living Data Program - 2025-05-25")

### addition of depth_code, column is populated based on depth_min_m and depth_max_m

data <- data %>%
  mutate(depth_code = case_when(
    depth_min_m == 2  & depth_max_m == 5  ~ 1,
    depth_min_m == 3  & depth_max_m == 5  ~ 2,
    depth_min_m == 5  & depth_max_m == 10 ~ 3,
    depth_min_m == 10 & depth_max_m == 15 ~ 4,
    depth_min_m == 15 & depth_max_m == 20 ~ 5,
    depth_min_m == 20 & depth_max_m == 30 ~ 6,
    depth_min_m == 30 & depth_max_m == 40 ~ 7,
    depth_min_m == 40 & depth_max_m == 50 ~ 8,
    depth_min_m == 50 & depth_max_m == 60 ~ 9,
    depth_min_m == 60 & depth_max_m == 70 ~ 10,
    depth_min_m == 70 & depth_max_m == 80 ~ 11,
    depth_min_m == 80 & depth_max_m == 90 ~ 12,
    depth_min_m == 90 & depth_max_m == 100 ~ 13
  ))

### splitting sounder_type and sounder_code into separate solumns

data <- data %>%
  extract(sounder_type, into = c("sounder_type", "sounder_code"), regex = "^(\\S+)\\s+(.*)$")

### renaming "gain" into "sounder_gain" and lake into "lake_name"

data <- data %>%
  rename(sounder_gain = gain)

data <- data %>%
  rename(lake = lake_name)

### rearranging columns

data <- data %>%
  relocate(depth_code, .before = 3)

data <- data %>%
  relocate(lake_code, .before = 1)

data <- data %>%
  relocate(lake, .before = 2)

data <- data %>%
  relocate(year, .before = 4)

data <- data %>%
  relocate(sounder_type, .before = 13)

data <- data %>%
  relocate(sounder_code, .before = 14)

data <- data %>%
  relocate(sounder_gain, .before = 15)

data <- data %>%
  relocate(source_file, .before = 16)


### printing and saving to a csv file

write.csv(data, "TARGET01_clean_V2.csv", row.names = FALSE)

###############################################################################
#### PART III: editing of the V2 of the converted .DAT file, this code includes,
#### addition of transect length and survey month

library(dplyr)
library(tidyverse)
library(lubridate)

target_data <- read.csv("TARGET01_clean_V3.csv") #input the file you are interested in editing
lake_strata <- read.csv("lake_strata_lengths.csv") #input the reference file 

### adding survey_month

target_data <- target_data %>%
  mutate(
    survey_date = ymd(survey_date),  #convert to date class
    month = month(survey_date)       #extract month as number
  )

# Add ats_year

target_data <- target_data %>%
  mutate(
    survey_date = as.Date(survey_date),
    ats_year = if_else(month(survey_date) >= 4,
                       year(survey_date),
                       year(survey_date) - 1)
  )


### before joining the files make sure that the columns are named properly and match between files

merged_data <- target_data %>%
  left_join(
    lake_strata[, c("lk.code", "depth_min", "depth_max", "Transect", "Area", "Length")],
    by = c(
      "lake_code" = "lk.code",
      "depth_min" = "depth_min",
      "depth_max" = "depth_max",
      "transect" = "Transect"
    )
  )

#### renaming columns
merged_data <- merged_data %>%
  rename(area = Area)

merged_data <- merged_data %>%
  rename(transect_length = Length)

merged_data <- merged_data %>%
  rename(survey_comments = survey_type)

### Save the file 
write.csv(merged_data, "TARGET01_clean_V3.csv", row.names = FALSE)

###############################################################################
#### PART IV: Target data issues, this code includes an addition of the data_issues column,
#### in the column issues found within the data are flagged

##### Code that deal with the Target 2006 data duplicate issues #########
library(tidyverse)
library(lubridate)

my_data <- read.csv("TARGET06_clean_V7.csv") 

### Convert data to character
my_data <- my_data %>%
  mutate(survey_date = as.character(survey_date))

### Deleting rows based on lake_code and survey_date, based on comments from email 
#### this assumes you already have the data_issues column, if not go to the bottom chunk of code 

my_data_filtered <- my_data %>%
  filter(!(lake_code == 228 & survey_date == "2007-02-15"),
         !(lake_code == 229 & survey_date %in% c("2004-02-04", "2007-02-15", "2004-02-04")))

### More editing based on comments made in the email  

my_data_modified <- my_data_filtered %>%
  mutate(
    data_issues = case_when(
      lake_code == 228 & survey_date == "2007-02-14" ~ paste0(
        coalesce(data_issues, ""), 
        if_else(str_detect(coalesce(data_issues, ""), "duplicate data dated 2007-02-15 deleted"), "", 
                "; duplicate data dated 2007-02-15 deleted")
      ),
      
      lake_code == 229 & survey_date == "2004-02-14" ~ paste0(
        "Survey not in ATS year; this was a duplicate, other copy deleted; probable true date Feb 14, 2007, from acoustic_survey_notes, data found in TARGET06.DAT; unknown why the target numbers are different than for date 2007-02-14"
      ),
      
      lake_code == 229 & survey_date == "2007-02-14" ~ paste0(
        coalesce(data_issues, ""), 
        if_else(str_detect(coalesce(data_issues, ""), "duplicate data dated 2007-02-15 deleted"), "", 
                "; duplicate data dated 2007-02-15 deleted")
      ),
      
      TRUE ~ data_issues
    )
  )

write.csv(my_data_modified, "TARGET06_clean_V8.csv", row.names = FALSE)

##### dealing with TARGET 1995 issues #######
#### Assumes you already have data issues column, if not then go to the bottom chunk of code 

library(tidyverse)

my_data <- read_csv("TARGET95_clean_V7.5.csv")

my_data <- my_data %>%
  mutate(
    data_issues = case_when(
      lake_code == 66 & survey_date == "1992-08-01" ~ 
        str_c(
          coalesce(data_issues, ""), 
          if_else(coalesce(data_issues, "") == "", "", "; "),
          "data erroneously stored in TARGET95.DAT, trawl_year updated from survey_date and processed_date"
        ),
      TRUE ~ data_issues
    )
  )

write_csv(my_data, "TARGET95_clean_V8.csv", row.names = FALSE)

#### DATA ISSUES FINAL CODE ####
#### This code creates the data_issues column and places all of the flags there

library(tidyverse)
library(lubridate)

my_data <- read.csv("TARGET07_clean_V7.csv")

my_data <- my_data %>%
  mutate(
    survey_year = as.numeric(as.character(survey_year)),
    ats_year = as.numeric(as.character(ats_year)),
    sounder_code = as.character(sounder_code),
    
    ### invalid dates ex. September 31st (not a calendar date)
    
    date_invalid = is.na(ymd(survey_date)),
    
    survey_date_parsed = ymd(survey_date),
    survey_year_from_date = year(survey_date_parsed),
    survey_month = month(survey_date_parsed),
    
    data_issues = paste(
      
      ### 1. NA proportions of sockeye and stickleback
      if_else(is.na(prop_sockeye) & is.na(prop_stickleback),
              "prop_sockeye and prop_stickleback = zero, set to NA", ""),
      
      ### 2. Invalid individual proportions of sockeye and stickleback are larger than 1
      if_else((!is.na(prop_sockeye) & prop_sockeye > 1) |
                (!is.na(prop_stickleback) & prop_stickleback > 1),
              "Invalid prop_sockeye and pro_stickleback proportions (>1)", ""),
      
      ### 3. Invalid SUM of proportions of sockeye and stickleback
      if_else(!is.na(prop_sockeye) & !is.na(prop_stickleback) &
                (prop_sockeye + prop_stickleback > 1),
              "Sum of prop_sockeye and pro_stickleback proportions > 1", ""),
      
      ### 4. Year-month check for survey_year and ats_year; the cut-off for ats_year is 
      ### that any samples between 01 April YYYY through 31 March YYYY+1 are designated ats_year YYYY
      if_else(
        (!is.na(survey_date_parsed)) &
          !((survey_year_from_date == ats_year & survey_month > 4) |
              (survey_year_from_date == ats_year + 1 & survey_month <= 4)),
        "Survey not in ATS year", ""),
      
      ### 5. Invalid numbers, negative numbers
      if_else(transect < 0 | depth_min_m < 0 | depth_max_m < 0 | targets < 0 |
                (!is.na(prop_sockeye) & prop_sockeye < 0) |
                (!is.na(prop_stickleback) & prop_stickleback < 0),
              "invalid numbers (negative values)", ""),
      
      ### 6. Transect = 0 but targets > 0
      if_else(transect_length_m == 0 & targets > 0,
              "transect_length = 0 but targets > 0", ""),
      
      ### 7. Invalid survey_date
      if_else(date_invalid, "missing or invalid survey_date", ""),
      
      sep = "; "
    ),
    
    data_issues = str_replace_all(data_issues, "^; |; $|; ;", ""),
    data_issues = if_else(data_issues == "", NA_character_, data_issues)
  ) %>%
  select(-date_invalid, -survey_date_parsed, -survey_year_from_date)


write.csv(my_data, "TARGET07_clean_V8.csv", row.names = FALSE)

#####################END OF SCRIPT#################

