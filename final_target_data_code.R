############################################################################
####### Full code in order to process the .dat TARGET files#################
############################################################################

library(tidyverse)
library(lubridate)

parse_target_dat <- function(filepath) {
  raw_lines <- read_lines(filepath)
  raw_lines <- raw_lines[trimws(raw_lines) != ""]  ### remove blank lines
  
  ### find the start of each data block
  header_indices <- which(str_detect(raw_lines, "depth / transect / targets"))
  all_data <- list()  ### to collect parsed chunks
  
  for (i in seq_along(header_indices)) {
    header_index <- header_indices[i]
    
    ### define metadata block
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
    
    ### get data lines for this chunk
    data_start <- header_index + 1
    data_end <- if (i < length(header_indices)) header_indices[i + 1] - 1 else length(raw_lines)
    data_lines <- raw_lines[data_start:data_end]
    
    ### parse each data line into a tibble
    parsed_data <- data_lines %>%
      str_trim() %>%                        ### removing white space
      discard(~ .x == "") %>%              ### skipping empty lines
      str_split("\\s+") %>%
      keep(~ length(.x) >= 5) %>%          ### keep only rows with >= 5 fields
      map(~ .x[1:5]) %>%
      map_dfr(~ tibble(
        depth = .x[1] %>% str_extract("^\\d+-\\d+"),
        transect = as.numeric(.x[2]),
        targets = as.numeric(.x[3]),
        pct_sockeye = as.numeric(.x[4]),
        pct_stickleback = as.numeric(.x[5])
      ))%>%
      mutate(
        lake = lake,
        lake_code = lake_code,
        survey_date = survey_date,
        sounder_type = sounder,
        gain = gain
      )
    
    all_data[[i]] <- parsed_data
  }
  
  ### combine all chunks
  final_data <- bind_rows(all_data)
  return(final_data)
}

### run the function
file_path <- "TARGET01.DAT"
target_data <- parse_target_dat(file_path)

### inspect and save
print(target_data)
write_csv(target_data, "TARGET01_clean.csv")

### To loop over multiple files use the following: ########################
### Set the directory, List the dat files using pattern = "\\.DAT$", using bind_rows function 
#### combined all the files into one (if interested)

####################VERSION 2 EDITING##############
#### Editing of data V2, after the conversion of .DAT file, now editing .CSV 
library(tidyverse)

data <- read_csv("TARGET01_clean.csv") 

### depth separation 
depth_separate <- data %>%
  mutate(depth = str_trim(depth)) %>%
  separate(depth, into = c("depth_min", "depth_max"), sep = "\\s*[-–]\\s*", convert = TRUE)
### depth_stratum is now separated into two different columns, one depth min and the other depth max

### separating and adding year and survey_type, had to add not specified later to avoid NA
library(dplyr)
library(tidyr)
library(lubridate)

data <- depth_separate %>%
  separate(lake, into = c("lake_name", "survey_comments"), sep = "\\s{2,}", extra = "merge") %>%
  mutate(survey_date = ymd(survey_date),
         year = year(survey_date),
         survey_comments = ifelse(is.na(survey_comments) | trimws(survey_comments) == "", 
                                  "NO COMMENTS", 
                                  survey_comments))

### renaming the column "notes" into "acoustic_survey_notes"

data <- data %>%
  rename(acoustic_survey_notes = notes)

### renaming sockeye/stickleback columns
data <- data %>%
  rename(prop_sockeye = pct_sockeye)

data <- data %>%
  rename(prop_stickleback = pct_stickleback)

### adding a column and populating with my info 

data <- data %>%
  mutate(program_notes = "YS - Living Data Program - 2025-05-25")

### adding a source file 

data <- data %>%
  mutate(source_file = "TARGET01.DAT")

### adding a column with depth_code, and then populating it based on depth_min and depth_max
library(dplyr)
library(tidyverse)

data <- data %>%
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
  ))

### splitting sounder_type into another column sounder_code 
library(tidyverse)

data <- data %>%
  extract(sounder_type, into = c("sounder_type", "sounder_code"), regex = "^(\\S+)\\s+(.*)$")

###renaming "gain" into "sounder_gain"

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
  relocate(survey_date, .before = 3)

data <- data %>%
  relocate(sounder_type, .before = 13)

data <- data %>%
  relocate(sounder_code, .before = 14)

data <- data %>%
  relocate(sounder_gain, .before = 15)

data <- data %>%
  relocate(source_file, .before = 16)

#### delete any NA rows 
library(dplyr)
library(tidyr)

data <- data %>% 
  drop_na()


### printing and saving to a csv file
print(data)
write.csv(data, "TARGET01_clean_V1.csv", row.names = FALSE)



#### To loop over all files use the following: 
###


####### Addition of transect length and survey month ######################
library(dplyr)
library(tidyverse)

### load your data, csv and reference file 
target_data <- read.csv("TARGET01_clean_V3.csv")
lake_strata <- read.csv("lake_strata_lengths.csv")

### before starting make sure that the columns are named properly

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
#### re-naming columns
merged_data <- merged_data %>%
  rename(area = Area)

merged_data <- merged_data %>%
  rename(transect_length = Length)

merged_data <- merged_data %>%
  rename(survey_comments = survey_type)

### adding survey month

target_data_issues <- target_data_issues %>%
  mutate(
    survey_date = ymd(survey_date),  # convert to Date class
    month = month(survey_date)       # extract month as number (1-12)
  )

write.csv(target_data_issues, "TARGET01_clean_V4.csv", row.names = FALSE)

##### Addition of trawl year #####################################
library(tidyverse)
library(lubridate)
library(assert)

my_data <- read_csv("TARGET01_clean_V4.csv")

# Add trawl_year or ats_year

my_data <- my_data %>%
  mutate(
    survey_date = ymd(survey_date),
    survey_year = year(survey_date),
    survey_month = month(survey_date),
    
    trawl_year = if_else(survey_month <= 4, survey_year - 1, survey_year),
  )

### check to see if data matches 
assert_that(all(target_data$survey_date >= 2001 & target_data$survey_date <= 2002))

# Save updated file
write_csv(df, "TARGET01_clean_V5.csv")

########### DATA ISSUES Flagging ################################

##### Script dealing with the Target 2006 data duplicate issues #########
library(tidyverse)
library(lubridate)

my_data <- read.csv("TARGET06_clean_V7.csv") 

### Convert to character
my_data <- my_data %>%
  mutate(survey_date = as.character(survey_date))

### Deleting rows based on lake_code and survey_date, based on comments from email 
#### this assumes you already have the data_issues column, if not go to the bottom chunk of code 

my_data_filtered <- my_data %>%
  filter(!(lake_code == 228 & survey_date == "2007-02-15"),
         !(lake_code == 229 & survey_date %in% c("2004-02-04", "2007-02-15")))

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
        "Year does not match trawl_year or trawl_year + 1; this was a duplicate, other copy deleted; probable true date Feb 14, 2007, from acoustic_survey_notes, data found in TARGET06.DAT; unknown why the target numbers are different than for date 2007-02-14"
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

##### STEP 2 dealing with TAGET 95 Issues mentioned in email #######
################### Target 95 issues ###########
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

write_csv(my_data, "TARGET95_clean_V8.csv")

####DATA ISSUES FINAL CODE ####
#### This code creates the data_issues column and places all of the flags there

library(tidyverse)
library(lubridate)

my_data <- read.csv("TARGET07_clean_V7.csv")

my_data <- my_data %>%
  rename(ats_year = trawl_year) %>%
  mutate(
    survey_year = as.numeric(as.character(survey_year)),
    ats_year = as.numeric(as.character(ats_year)),
    sounder_code = as.character(sounder_code),
    
    ### invalid dates like Sept 31
    date_invalid = is.na(ymd(survey_date)),
    
    survey_date_parsed = ymd(survey_date),
    survey_year_from_date = year(survey_date_parsed),
    survey_month = month(survey_date_parsed),
    
    data_issues = paste(
      
      ### 1. NA proportions of sockeye and stickleback
      if_else(is.na(prop_sockeye) & is.na(prop_stickleback),
              "prop_sockeye and prop_stickleback = zero, set to NA", ""),
      
      ### 2. Invalid individual proportions of sockeye and stickleback
      if_else((!is.na(prop_sockeye) & prop_sockeye > 1) |
                (!is.na(prop_stickleback) & prop_stickleback > 1),
              "Invalid prop_sockeye and pro_stickleback proportions (>1)", ""),
      
      ### 3. Invalid sum of proportions of sockeye and stickleback
      if_else(!is.na(prop_sockeye) & !is.na(prop_stickleback) &
                (prop_sockeye + prop_stickleback > 1),
              "Sum of prop_sockeye and pro_stickleback proportions > 1", ""),
      
      ### 4. Year-month check for trawl_year and ats_year
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
