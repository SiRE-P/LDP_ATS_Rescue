library(tidyverse)
library(tools)
library(lubridate)

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

### depth separation 
depth_separate <- all_target_data %>%
  dplyr::mutate(depth = str_trim(depth)) %>%
  separate(depth, into = c("depth_min", "depth_max"), sep = "\\s*[-–]\\s*", convert = TRUE)

data <- depth_separate %>%
  separate(lake,
           into = c("lake_name", "survey_comments"),
           sep = "\\s{2,}",
           extra = "merge",
           fill = "right") %>% 
  mutate(survey_date = ymd(survey_date),
         year = year(survey_date),
         survey_comments = ifelse(is.na(survey_comments) | trimws(survey_comments) == "", 
                                  "NO COMMENTS", 
                                  survey_comments))


### renaming sockeye/stickleback columns
data <- data %>%
  rename(prop_sockeye = pct_sockeye) %>% 
  rename(prop_stickleback = pct_stickleback)

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

data <- data %>%
  extract(sounder_type, into = c("sounder_type", "sounder_code"), regex = "^(\\S+)\\s+(.*)$")

data <- data %>%
  rename(sounder_gain = gain) %>% 
  rename(lake = lake_name)

### rearranging columns
data <- data %>%
  select(
    lake_code,
    lake,
    survey_date,
    year,
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
  )

##### Addition of ats year
data <- data %>%
  mutate(
    survey_date = as.Date(survey_date),
    ats_year = if_else(month(survey_date) >= 4,
                       year(survey_date),
                       year(survey_date) - 1)
  )

### Convert to character
data <- data %>%
  mutate(survey_date = as.character(survey_date))

lake_strata <- read.csv("./data/lake_transect_stratum_lengths.csv") #input the reference file 

merged_data <- data %>%
  left_join(
    lake_strata %>%
      rename(lake = Lake, lake_code = LakeCode, transect = Transect) %>%
      mutate(transect = str_extract(transect, "\\d+") %>% as.numeric()),
    by = c("lake", "lake_code", "transect"), 
    relationship = "many-to-many"
  )

#### renaming columns
merged_data <- merged_data %>%
  rename(area = Area) %>% 
  rename(transect_length = Length) 
