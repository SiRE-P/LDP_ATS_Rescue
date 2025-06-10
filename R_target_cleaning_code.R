#### .DAT target data information extracting and cleaning to store into a .CSV
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

#####################END OF SCRIPT#################

####################VERSION 2 EDITING##############
#### Editing of data V2, after the conversion of .DAT file, now editing .CSV 
library(tidyverse)

data <- read_csv("TARGET01_clean.csv") 
###data <- data[-1, ] ### delete the first row, ONLY WHEN NEEDED 


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

#####################END OF SCRIPT#################
