### Addition of transect length, area, and addition of their units 
library(dplyr)
library(tidyverse)

### load your data, csv and reference file 
target_data <- read.csv("TARGET01_clean_V3.csv")
lake_strata <- read.csv("lake_strata_lengths.csv")

### before starting make sure that the columns are named properly, and you know the names

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
  rename(sounder_gain = weather)

merged_data <- merged_data %>%
  rename(survey_comments = survey_type)

### clean the sounder_type column, noticed spaces and uneven format

### clean the column
merged_data$sounder_code <- ifelse(
  merged_data$sounder_type == "FURUNO",
  gsub("FM ?22.*", "FM22", merged_data$sounder_code),
  merged_data$sounder_code
)

### clean the survey_comments column 
library(dplyr)

merged_data <- merged_data %>%
  mutate(survey_comments = ifelse(survey_comments == "NOT SPECIFIED", 
                                  "NO COMMENTS", 
                                  survey_comments))
merged_data <- merged_data %>%
  mutate(sounder_code = ifelse(sounder_code == "FM 22         1", 
                               "FM22",sounder_code))


### rearrange 
merged_data <- merged_data %>%
  relocate(depth_code, .before = 3)

merged_data <- merged_data %>%
  relocate(lake_code, .before = 1)

merged_data <- merged_data %>%
  relocate(lake, .before = 2)

merged_data <- merged_data %>%
  relocate(year, .before = 4)

merged_data <- merged_data %>%
  relocate(survey_date, .before = 3)

merged_data <- merged_data %>%
  relocate(sounder_type, .before = 13)

merged_data <- merged_data %>%
  relocate(sounder_code, .before = 14)

merged_data <- merged_data %>%
  relocate(sounder_gain, .before = 15)

merged_data <- merged_data %>%
  relocate(source_file, .before = 16)

merged_data <- merged_data %>%
  relocate(transect_length, .before = 9)

merged_data <- merged_data %>%
  relocate(area, .before = 10)


write.csv(merged_data, "TARGET01_clean_V4.csv", row.names = FALSE)

##### END OF SCRIPT #######

