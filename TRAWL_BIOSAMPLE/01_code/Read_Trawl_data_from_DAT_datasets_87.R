###############################################################################
##############           TRAWL data code 1987.              ###################
##############    Converting and cleaning SAS trawl files   ###################
############## Authors: Alice Assmar (McGill Uni.), David   ###################
############## Hunt (McGill Uni.),  Yuliya Shtymburski      ###################
############## (U. Regina), Howard Stiff (DFO Nanaimo)      ###################
##############        Athena Ogden (DFO Nanaimo)            ###################
###############################################################################

# getwd()
# setwd("/LDP_ATS_Rescue")

# Install necessary packages if they are not yet installed
packages <- c("beepr", "dplyr", "lubridate","progress",
              "purrr","stringr", "tibble", "tictoc", "tidyverse", "tools", "Rcpp", "haven")
install.packages(setdiff(packages, row.names(installed.packages())))

# Load necessary packages
{
  library(beepr)
  library(dplyr)
  library(lubridate)
  library(purrr)
  library(stringr)
  library(tibble)       # install.packages("tictoc")
  library(tictoc)       # get elapsed time 
  library(tidyverse)
  library(tools)
  library(Rcpp) 
  library(haven) # read SAS files
}

################################  Step 1  #####################################
########################## Organize the directory #############################

# Create the directories to hold output files
if (!dir.exists("./TRAWL_BIOSAMPLE/02_intermediate_out")) {dir.create("./TRAWL_BIOSAMPLE/02_intermediate_out")
} else {message("Directory already exists.")} # ensure CSV output directory exists
if (!dir.exists("./TRAWL_BIOSAMPLE/03_errors_out")) {dir.create("./TRAWL_BIOSAMPLE/03_errors_out")
} else {message("Directory already exists.")} # ensure CSV output directory exists
if (!dir.exists("./TRAWL_BIOSAMPLE/04_final_output")) {dir.create("./TRAWL_BIOSAMPLE/04_final_output")
} else {message("Directory already exists.")}  # ensure plot output directory exists
if (!dir.exists("./TRAWL_BIOSAMPLE/05_ARCHIVE")) {dir.create("./TRAWL_BIOSAMPLE/05_ARCHIVE")
}  else {message("Directory already exists.")} # ensure archive directory exists for storing date-stamped copy of output
if (!dir.exists("./TRAWL_BIOSAMPLE/06_Figures")) {dir.create("./TRAWL_BIOSAMPLE/06_Figures")
}  else {message("Directory already exists.")} # ensure figure directory exists for storing plots and tables

# Create variable to hold output directory and the target file
input_folder <- "./TRAWL_BIOSAMPLE/00_raw_data/01_DAT"
intermediate_out_folder <- "./TRAWL_BIOSAMPLE/02_intermediate_out"
trawl_file <- "trawl87"

################################  Step 2  #####################################
################### Read dat and loop over data files #########################

### input the dat file you are interested in cleaning
lines <- readLines(paste0(input_folder, "/", trawl_file,".dat"))

### remove empty lines and trim whitespace, except for years 86-99
#lines <- lines[str_trim(lines) != ""] 
#lines <- lines[str_trim(lines) != "--"] 

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
  first_number <- clean_line(sub(" .*", "", lines[i])) # Trawl89 has two numbers for total of fish, added this to select only the first number.

  #num_fish <- as.integer(clean_line(lines[i]))
  num_fish <- as.integer(first_number)
  if (is.na(num_fish)) num_fish <- 0
  i <- i + 1
  
  ### check if enough lines remain for all fish data
  if (i + 4*num_fish - 1 > line_count) break
  
  for (j in 1:num_fish) {
    length_mm    <- clean_line(lines[i])
    line_index   <- i
    weight_g     <- clean_line(lines[i + 1])
    scale_number <- clean_line(lines[i + 2])
    # Working aroung the blocks of data with only three lines instead of four
    fish_letter  <- ifelse(is.na(clean_line(lines[i + 3])) | grepl("[[:alpha:]]|^\\d$", clean_line(lines[i + 3])), clean_line(lines[i + 3]), NA)
    # If the fourth line is missing, then ask the loop to return one line, so when it sums up at the end, it will be the correct line
    is_missing   <- ifelse(!is.na(clean_line(lines[i + 3])) & grepl("\\d{2,}", clean_line(lines[i + 3])), TRUE, FALSE) 
    if (is_missing == TRUE) {
      i <- i - 1
    }
    
    #fish_letter <- ifelse(!is.na(clean_line(lines[i + 3])) & grepl("[:alpha:]", clean_line(lines[i + 3])), clean_line(lines[i + 3]), NA)
    
    records[[length(records) + 1]] <- tibble(
      process_date, processor, lake_code, trawl_date, sample_number,
      sample_type, trawl_number, start_end_time, duration_min, depth_m, species_code,
      trawl_location, preservative_code,
      fish_length = as.numeric(length_mm),
      fish_weight = as.numeric(weight_g),
      scale_book  = scale_number,
      fish_id     = j,
      fish_total  = num_fish,
      scale_book_letter = as.character(fish_letter),
      source_line = line_index
    )
    
    i <- i + 4 # the fish data is in sets of four
  }
}

### If step 3 has a lot of errors, that means that the format of the .dat file is 
### inconsistent. Meaning that the format of the .dat file is not 13 lines 
### of metadata and 3 sets of data per fish. Issues that cause errors in code include:
### scale book letter (A,B,C,etc.), empty rows, and missing metadata lines

### combine and save 
final_df <- bind_rows(records)

write.csv(final_df, paste0(intermediate_out_folder, "/", trawl_file, "_DAT.csv"), row.names = FALSE)

###############################################################################
######## Step 3: Editing of the version 1 of the csv ########################
### This code is meant to edit the first converted version of the .csv file, splitting 
#### columns and data cleaning, etc. 

#final_df <- read.csv(paste0(intermediate_out_folder, "/", trawl_file, "_DAT.csv"))

### trawl number column removing comments and placing into separate column 

final_df <- final_df %>%
  mutate(
    ### extract the digits at the start
    trawl_number_clean = str_extract(trawl_number, "^\\d{1,2}"),
    
    ### extract the rest of the string as comment
    trawl_number_comment = str_trim(str_remove(trawl_number, "^\\d{1,2}")),
    
    ### replace missing/empty comments with ""
    trawl_number_comment = if_else(trawl_number_comment == "" | is.na(trawl_number_comment),
                                   "",
                                   trawl_number_comment),
    
    ### convert number to integer (optional, based on your needs)
    trawl_number = as.integer(trawl_number_clean)
  ) %>%
  select(-trawl_number_clean)

### trawl date column removing comments and placing into separate column
final_df <- final_df %>%
  mutate(
    ### extract the digits at the start
    trawl_date_clean = str_extract(trawl_date, "^\\d{5,6}"),
    
    ### extract the rest of the string as comment
    trawl_date_comment = str_trim(str_remove(trawl_date, "^\\d{5,6}")),
    
    ### replace missing/empty comments with ""
    trawl_date_comment = if_else(trawl_date_comment == "" | is.na(trawl_date_comment),
                                 "",
                                 trawl_date_comment),
    
    ### convert number to integer (optional, based on your needs)
    trawl_date = as.integer(trawl_date_clean)
  ) %>%
  select(-trawl_date_clean)

### editing trawl_date and process_date into ISO format 
final_df <- final_df %>%
  mutate(
    process_date = str_pad(as.character(process_date), 6, pad = "0"),
    trawl_date  = str_pad(as.character(trawl_date), 6, pad = "0"),
    process_date = dmy(process_date),
    trawl_date  = dmy(trawl_date),
    process_date = format(process_date, "%Y-%m-%d"),
    trawl_date  = format(trawl_date, "%Y-%m-%d")
  )

## Add leading '0' that were removed by the software used to write the .dat files
final_df$start_end_time <- str_pad(final_df$start_end_time, 4, "left", pad = "0")

## Convert the hour "HHMM" into "HH:MM:SS" as in the SAS files
final_df$start_end_time <- sprintf("%s:%s:00", substr(final_df$start_end_time, 1, 2), substr(final_df$start_end_time, 3, 4))

### species code column removing comments and placing into separate column
final_df <- final_df %>%
  mutate(
    ### extract the digits at the start
    species_code_clean = str_extract(species_code, "^\\d{1,2}"),
    
    ### extract the rest of the string as comment
    species_code_comment = str_trim(str_remove(species_code, "^\\d{1,2}")),
    
    ### replace missing/empty comments with ""
    species_code_comment = if_else(species_code_comment == "" | is.na(species_code_comment),
                                   "",
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
    
    ### replace missing/empty comments with NA
    preservative_code_comment = if_else(preservative_code_comment == "" | is.na(preservative_code_comment),
                                        "",
                                        preservative_code_comment),
    
    ### convert number to integer (optional, based on your needs)
    preservative_code = as.integer(preservative_code_clean)
  ) %>%
  select(-preservative_code_clean)

#### processor column removing comments
final_df <- final_df %>%
  mutate(
    ### extract the digits at the start
    processor_clean = str_extract(processor, "^\\d{1,2}"),
    ### convert number to integer (optional, based on your needs)
    processor = as.integer(processor_clean)
  ) %>%
  select(-processor_clean)

### trimming the lake_code column to not have any words just numbers
final_df <- final_df %>%
  mutate(lake_code = str_extract(lake_code, "\\d+"))

### trimming the depth_m column to not have any words just numbers
final_df <- final_df %>%
  mutate(
    ### extract the digits at the start
    depth_m_clean = str_extract(depth_m, "^\\d+"),
    
    ### extract the rest of the string as comment
    depth_m_comment = str_trim(str_remove(depth_m, "^\\d+")),
    
    ### replace missing/empty comments with NA
    depth_m_comment = if_else(depth_m_comment == "" | is.na(depth_m_comment),
                                        "",
                                        depth_m_comment),
    
    ### convert number to integer (optional, based on your needs)
    depth_m = as.integer(depth_m_clean)
  ) %>%
  select(-depth_m_clean)

### Adding look up tables, that were provided in the Sharepoint
### Look up tables include: lake name, fish species, preservative code,
### weight convertion formula 

### load your look-up tables
fish_species_code_lookup_table <- read.csv("./TRAWL_BIOSAMPLE/00_raw_data/04_YS_look_up_tables/fish_species_code_lookup_table.csv")

preservative_code_lookup_table <- read.csv("./TRAWL_BIOSAMPLE/00_raw_data/04_YS_look_up_tables/preservative_code_lookup_table.csv") 

preservative_code_weight_conversion_lookup <- read.csv("./TRAWL_BIOSAMPLE/00_raw_data/04_YS_look_up_tables/preservative_code_weight_conv_lookup_table.csv")

lake_name <- read.csv("./TRAWL_BIOSAMPLE/00_raw_data/04_YS_look_up_tables/lake_name_table.csv")

aging_technique_lookup_table <- data.frame(aging_technique = as.character(c(0, 1, 2, 3, 4, 5)), 
                                           aging_technique_name = c("No scale", "Scale & aged", "Scale - Poor", "Scale - Regen.", 
                                                                    "Scale - Unsure", "Defaulted"))

# check and add new columns to standardize the columns in the spreadsheet
new_columns <- c("process_date", "sample_number", 
                 "scale", "scale_book",
                 "age", "aging_technique", "end_time")

for (columns in new_columns) {
  if (!columns %in% names(final_df)) {
    final_df[[columns]] <- NA 
  }
}

### make sure the types of data match
final_df <- final_df %>%
  mutate(
    preservative_code = as.character(preservative_code),
    scale = as.character(scale),
    age = as.character(age),
    aging_technique = as.character(aging_technique),
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
  left_join(lake_name, by = "lake_code") %>%
  left_join(aging_technique_lookup_table, by = "aging_technique")

### make sure the types of data match
final_df <- final_df %>%
  mutate(aging_technique_name = as.character(aging_technique_name))

### add a source file 
final_df <- final_df %>%
  mutate(source_file = paste0(trawl_file,".dat"))

### adding a column and populating with my info 
final_df <- final_df %>%
  mutate(program_notes = "AA - Living Data Program - 2025")

### renaming columns
# Cleaning columns' names
rename_map <- c("process_date" = "procdate", 
                "processor" = "observer.x", 
                "processor" = "observer",
                "lake_code" = "system", 
                "trawl_date" = "date", 
                "sample_number" = "sample", 
                "trawl_number" = "trwlnmbr", 
                "start_time" = "statime",
                "start_time" = "start_end_time",
                "end_time" = "endtime",
                "duration_mi" = "duration_min", 
                "depth_m" = "depth", 
                "species_code" = "fspecies",
                "aging_technique" = "aged", 
                "preservative_code" = "prsrv", 
                "fish_length_mm" = "fish_length",
                "fish_weight_g" = "fish_weight",
                "standardized_weight_g" = "stdwght",
                "scale_book" = "sclbook",
                "fish_id" = "fishnmbr",
                "fish_total" = "nmbrfish",
                "lake_name" = "lake")

# Match with the columns present in the dataframe
rename_map <- rename_map[rename_map %in% names(final_df)]
# Rename columns
final_df <- final_df %>%  
  rename(!!!rename_map)

### adding ats_year and trawl_month
final_df <- final_df %>%
  mutate(
    trawl_date = ymd(trawl_date),
    trawl_month = month(trawl_date),
    ats_year = year(trawl_date)
  )

### Creating unique IDs for fishes and Trawls
final_df$trawl_unique_ID <- paste(final_df$trawl_date, final_df$lake_code, final_df$trawl_number, final_df$depth_m, sep = "_")
final_df$fish_unique_ID <- paste(final_df$trawl_date, final_df$lake_code, final_df$trawl_number, final_df$depth_m, final_df$species_code, final_df$fish_id, final_df$fish_weight_g, final_df$fish_length_mm, sep = "_")

# Reorganize columns order
final_df <- final_df %>% 
  relocate(process_date, processor,  
           lake_code, lake_name, trawl_date,
           start_time, end_time, duration_mi, depth_m,  
           trawl_number, fish_total, fish_id, species_code, fish_description,
           fish_length_mm, fish_weight_g, weight_conversion_formula, 
           preservative_code, preservative_description, sample_number, 
           scale, scale_book, age, aging_technique, aging_technique_name, source_file, source_line)

### Checking for duplicates
# Check for duplicates based on selected columns
duplicate_rows_indices <- final_df[duplicated(final_df$fish_unique_ID), ]

# View the duplicate rows
print(duplicate_rows_indices)

all_duplicates <- final_df %>%
  group_by(fish_unique_ID, species_code_comment) %>%
  filter(n() > 1) %>%
  arrange(fish_unique_ID) %>%
  ungroup()

# Remove duplicates
final_df <- final_df[!duplicated(final_df[c("fish_unique_ID", "species_code_comment")]), ]

### save 
write.csv(final_df, paste0(intermediate_out_folder, "/", trawl_file, "_DAT.csv"), row.names = FALSE)













