################## TRAWL data code 1984-1999 ##################################
############## Part I: converting .dat trawl file into .csv ###################
###############################################################################
library(tidyverse)

### input the dat file you are interested in cleaning
lines <- readLines("trawl87_cleaned.dat")

### remove empty lines and trim whitespace
lines <- lines[str_trim(lines) != ""]

### function to remove comments and trim
clean_line <- function(line) {
  str_trim(str_split(line, "#", simplify = TRUE)[1])
}

### initialize variables
records <- list()
i <- 1
line_count <- length(lines)

### loop through the file
while (i <= line_count) {
  
  ### check if there are at least 13 lines for metadata
  if (i + 12 > line_count) break
  metadata <- map_chr(lines[i:(i + 12)], clean_line)
  
  process_date      <- metadata[1]
  processor         <- metadata[2]
  lake_code         <- metadata[3]
  trawl_date        <- metadata[4]
  sample_number     <- metadata[5]
  sample_type       <- metadata[6]
  trawl_number      <- metadata[7]
  start_end_time    <- metadata[8]
  duration_min      <- metadata[9]
  depth_m           <- metadata[10]
  species_code      <- metadata[11]
  trawl_location    <- metadata[12]
  preservative_code <- metadata[13]
  
  i <- i + 13
  
  ### check if there is a line for num_fish
  if (i > line_count) break
  num_fish <- as.integer(clean_line(lines[i]))
  if (is.na(num_fish)) num_fish <- 0
  i <- i + 1
  
  ### check if enough lines remain for all fish data
  if (i + 3*num_fish - 1 > line_count) break
  
  for (j in 1:num_fish) {
    length_mm    <- clean_line(lines[i])
    weight_g     <- clean_line(lines[i + 1])
    scale_number <- clean_line(lines[i + 2])
    
    records[[length(records) + 1]] <- tibble(
      process_date, processor, lake_code, trawl_date, sample_number,
      sample_type, trawl_number, start_end_time, duration_min, depth_m, species_code,
      trawl_location, preservative_code,
      fish_length = as.numeric(length_mm),
      fish_weight = as.numeric(weight_g),
      scale_book  = scale_number,
      fish_id     = j,
      fish_total  = num_fish
    )
    
    i <- i + 3 # the fish data is in sets of three
  }
}
### If step 3 has a lot of errors, that means that the format of the .dat file is 
### inconsistent. Meaning that the format of the .dat file is not 13 lines 
### of metadata and 3 sets of data per fish. Issues that cause errors in code include:
### scale book letter (A,B,C,etc.), empty rows, and missing metadata lines

### combine and save 
final_df <- bind_rows(records)

write.csv(final_df, "trawl_87_V1.csv", row.names = FALSE)

###############################################################################
######## Part II: Editing of the version 1 of the csv ########################
### This code is meant to edit the first converted version of the .csv file, splitting 
#### columns and data cleaning, etc. 

library(tidyverse)
library(lubridate)
library(stringr)

### editing trawl_date and process_date into ISO format 

final_df <- read.csv("trawl_87_V1.csv") %>%
  mutate(
    process_date = str_pad(as.character(process_date), 6, pad = "0"),
    trawl_date  = str_pad(as.character(trawl_date), 6, pad = "0"),
    process_date = dmy(process_date),
    trawl_date  = dmy(trawl_date),
    process_date = format(process_date, "%Y-%m-%d"),
    trawl_date  = format(trawl_date, "%Y-%m-%d")
  )


### trawl number column removing comments and placing into separate column 

final_df <- final_df %>%
  mutate(
    ### extract the digits at the start
    trawl_number_clean = str_extract(trawl_number, "^\\d{1,2}"),
    
    ### extract the rest of the string as comment
    trawl_number_comment = str_trim(str_remove(trawl_number, "^\\d{1,2}")),
    
    ### replace missing/empty comments with "NO COMMENTS"
    trawl_number_comment = if_else(trawl_number_comment == "" | is.na(trawl_number_comment),
                                   "NO COMMENT",
                                   trawl_number_comment),
    
    ### convert number to integer (optional, based on your needs)
    trawl_number = as.integer(trawl_number_clean)
  ) %>%
  select(-trawl_number_clean)


### species code column removing comments and placing into separate column
final_df <- final_df %>%
  mutate(
    ### extract the digits at the start
    species_code_clean = str_extract(species_code, "^\\d{1,2}"),
    
    ### extract the rest of the string as comment
    species_code_comment = str_trim(str_remove(species_code, "^\\d{1,2}")),
    
    ### replace missing/empty comments with "NO COMMENTS"
    species_code_comment = if_else(species_code_comment == "" | is.na(species_code_comment),
                                   "NO COMMENT",
                                   species_code_comment),
    
    ### convert number to integer (optional, based on your needs)
    species_code = as.integer(species_code_clean)
  ) %>%
  select(-species_code_clean)


#### preservation code column removing comments and placing into separate column
final_df <- final_df %>%
  mutate(
    ### extract the digits at the start
    preservative_code_clean = str_extract(preservative_code, "^\\d{1,2}"),
    
    ### extract the rest of the string as comment
    preservative_code_comment = str_trim(str_remove(preservative_code, "^\\d{1,2}")),
    
    ### replace missing/empty comments with "NO COMMENTS"
    preservative_code_comment = if_else(preservative_code_comment == "" | is.na(preservative_code_comment),
                                        "NO COMMENT",
                                        preservative_code),
    
    ### convert number to integer (optional, based on your needs)
    preservative_code = as.integer(preservative_code_clean)
  ) %>%
  select(-preservative_code_clean)


### trimming the lake_code column to not have any words just numbers
final_df <- final_df %>%
  mutate(lake_code = str_extract(lake_code, "\\d+"))


### Adding look up tables, that were provided in the Sharepoint
### Look up tables include: lake name, fish species, preservative code,
### weight convertion formula 

### load your look-up tables
fish_species_code_lookup_table <- read.csv("look_up_tables/fish_species_code_lookup_table.csv")

preservative_code_lookup_table <- read.csv("look_up_tables/preservative_code_lookup_table.csv") 

preservative_code_weight_conversion_lookup <- read.csv("look_up_tables/preservative_code_weight_conv_lookup_table.csv")

lake_name <- read.csv("look_up_tables/lake_name_table.csv")

### make sure the types of data match
final_df <- final_df %>%
  mutate(
    preservative_code = as.character(preservative_code),
    lake_code = as.character(lake_code)
  )

fish_species_code_lookup_table <- fish_species_code_lookup_table %>%
  mutate(species_code = as.integer(species_code))

preservative_code_lookup_table <- preservative_code_lookup_table %>%
  mutate(preservative_code = as.character(preservative_code))

preservative_code_weight_conversion_lookup <- preservative_code_weight_conversion_lookup %>%
  mutate(preservative_code = as.character(preservative_code))

lake_name <- lake_name %>%
  mutate(lake_code = as.character(lake_code))

### join the tables
final_df <- final_df %>%
  left_join(fish_species_code_lookup_table, by = "species_code") %>%
  left_join(preservative_code_lookup_table, by = "preservative_code") %>%
  left_join(preservative_code_weight_conversion_lookup, by = "preservative_code") %>%
  left_join(lake_name, by = "lake_code")

### add a source file 
final_df <- final_df %>%
  mutate(source_file = "trawl87.dat")

### adding a column and populating with my info 
final_df <- final_df %>%
  mutate(program_notes = "YS - Living Data Program - 2025-08-11")

### renaming columns
final_df <- final_df %>%
  rename(species_description = fish_description)

final_df <- final_df %>%
  rename(total_fish = fish_total)

final_df <- final_df %>%
  rename(scale_book_number = scale_book)

final_df <- final_df %>%
  rename(fish_weight_g = fish_weight)

final_df <- final_df %>%
  rename(fish_length_mm = fish_length)


### adding ats_year and trawl_month
final_df <- final_df %>%
  mutate(
    trawl_date = ymd(trawl_date),
    trawl_month = month(trawl_date),
    ats_year = year(trawl_date)
  )


### adding fish weight convertion formulas based on the preservation code

final_df <- final_df %>%
  mutate(
    fish_std_weight = case_when(
      preservative_code == 0  ~ fish_weight_g,
      preservative_code == 1  ~ fish_weight_g / 1.115100,
      preservative_code == 2  ~ fish_weight_g / 0.868429,
      preservative_code == 3  ~ fish_weight_g / 0.832408,
      preservative_code == 4  ~ fish_weight_g / 1.075312,
      preservative_code == 96 ~ fish_weight_g / 1.116898,
      preservative_code == 97 ~ fish_weight_g / 1.075312,
      preservative_code == 99 ~ fish_weight_g / 1.115100,
      preservative_code %in% c(95, 98) ~ NA_real_,
      TRUE ~ NA_real_ 
    )
  )

### adding weight conversion factor column 

final_df <- final_df %>%
  mutate(
    weight_conversion_factor = case_when(
      preservative_code == 0  ~ fish_weight_g,
      preservative_code == 1  ~ 1.115100,
      preservative_code == 2  ~ 0.868429,
      preservative_code == 3  ~ 0.832408,
      preservative_code == 4  ~ 1.075312,
      preservative_code == 96 ~ 1.116898,
      preservative_code == 97 ~ 1.075312,
      preservative_code == 99 ~ 1.115100,
      preservative_code %in% c(95, 98) ~ NA_real_,
      TRUE ~ NA_real_
    )
  )

### rearranging columns 
final_df <- final_df %>%
  relocate(lake_code, .before = 1)

final_df <- final_df %>%
  relocate(lake, .before = 2)

final_df <- final_df %>%
  relocate(ats_year, .before = 3)

final_df <- final_df %>%
  relocate(trawl_month, .before = 4)

final_df <- final_df %>%
  relocate(trawl_date, .before = 5)

final_df <- final_df %>%
  relocate(trawl_date_comment, .before = 6)

final_df <- final_df %>%
  relocate(trawl_number, .before = 7)

final_df <- final_df %>%
  relocate(trawl_location, .before = 8)

final_df <- final_df %>%
  relocate(depth_m, .before = 9)

final_df <- final_df %>%
  relocate(start_end_time, .before = 10)

final_df <- final_df %>%
  relocate(duration_min, .before = 11)

final_df <- final_df %>%
  relocate(trawl_number_comment, .before = 12)

final_df <- final_df %>%
  relocate(sample_number, .before = 13)

final_df <- final_df %>%
  relocate(sample_type, .before = 14)

final_df <- final_df %>%
  relocate(total_fish, .before = 15)

final_df <- final_df %>%
  relocate(fish_id, .before = 16)

final_df <- final_df %>%
  relocate(species_code, .before = 17)

final_df <- final_df %>%
  relocate(species_description, .before = 18)

final_df <- final_df %>%
  relocate(species_code_comment, .before = 19)

final_df <- final_df %>%
  relocate(fish_length_mm, .before = 20)

final_df <- final_df %>%
  relocate(fish_weight_g, .before = 21)

final_df <- final_df %>%
  relocate(fish_std_weight, .before = 22)

final_df <- final_df %>%
  relocate(preservative_code, .before = 23)

final_df <- final_df %>%
  relocate(preservative_description, .before = 24)

final_df <- final_df %>%
  relocate(weight_conversion_formula, .before = 25)

final_df <- final_df %>%
  relocate(weight_conversion_factor, .before = 26)

final_df <- final_df %>%
  relocate(scale_book_number, .before = 27)

final_df <- final_df %>%
  relocate(process_date, .before = 28)

final_df <- final_df %>%
  relocate(processor, .before = 29)

final_df <- final_df %>%
  relocate(source_file, .before = 29)

final_df <- final_df %>%
  relocate(program_notes, .before = 31)

##############################################################################
############## PART III: trawl data issues ###################################
##### trawl data issues, for now it is checking for negative values in all columns,
##### creates a data_issues column where the issues are flagged

final_df <- final_df %>%
  mutate(
    
    ### date format check: must be yyyy-mm-dd (ISO format)
    invalid_trawl_date = !str_detect(trawl_date, "^\\d{4}-\\d{2}-\\d{2}$"),
    invalid_process_date = !str_detect(process_date, "^\\d{4}-\\d{2}-\\d{2}$"),
    
    ### negative value checks
    negative_depth = depth_m < 0,
    negative_length = fish_length_mm < 0,
    negative_weight = fish_weight_g < 0,
    negative_std_weight = fish_std_weight < 0,
    negative_conversion = weight_conversion_formula < 0,
    
    ### adding trawl_issues column with all detected issues
    trawl_data_issues = paste(
      if_else(invalid_trawl_date, "invalid trawl_date", ""),
      if_else(invalid_process_date, "invalid process_date", ""),
      if_else(negative_depth, "negative depth_m", ""),
      if_else(negative_length, "negative fish_length", ""),
      if_else(negative_weight, "negative fish_weight", ""),
      if_else(negative_std_weight, "negative fish_std_weight", ""),
      if_else(negative_conversion, "negative weight_conversion_formula", ""),
      sep = "; "
    ) %>%
      str_replace_all("^;\\s*|;\\s*$", "") %>%  # trim trailing/leading semicolons
      na_if("")                                 # convert empty string to NA
  ) %>%
  select(-starts_with("invalid_"), -starts_with("negative_"))  # Optional cleanup


write.csv(final_df, "trawl87_V3.csv", row.names = FALSE)


###############################################################################
###### PART IV: Code for cross checking between the access and apple data #####
###############################################################################

library(dplyr)
library(lubridate)
library(readr)
library(stringr)

access_raw <- read.csv("cross_check_84_86_V2.csv")

### load the main dataset (trawl_84_86.csv)
trawl_84_86 <- read_csv("TRAWL_84_86.csv") %>%
  mutate(
    trawl_date = as.Date(trawl_date),
    ats_year = year(trawl_date),
    lake_code = as.character(lake_code), 
    trawl_number = as.numeric(trawl_number),
    fish_id = as.numeric(fish_id)
  )

### clean and prep the access dataset, also make sure the names of clumns match
access_clean <- access_raw %>%
  mutate(
    trawl_date = as.Date(trawl_date),
    ats_year = year(trawl_date),
    lake_code = as.character(lake_code), 
    trawl_number = as.numeric(trawl_number),
    fish_id = as.numeric(fish_id)
  ) %>%
  select(lake_code, lake, ats_year, trawl_month, trawl_date) %>%
  distinct()

### flag Access rows
access_clean <- access_clean %>%
  mutate(in_access = TRUE)

### cross-check join 
trawl_checked <- trawl_84_86 %>%
  left_join(access_clean,
            by = c("lake_code", "lake", "ats_year", "trawl_month", "trawl_date")) %>%
  mutate(
    cross_check = if_else(is.na(in_access), "Not Matched: found in Apple only", "Matched")
  )

write.csv(trawl_checked, "trawl_84_86_with_crosscheck.csv", row.names = FALSE)


######## END ##############

