##############
####    TRAWL data                
####    Assembling .csv trawl files       
####    Authors: Alice Assmar (McGill Uni.), David Hunt (McGill Uni.),
####    Howard Stiff (DFO Nanaimo), Athena Ogden (DFO Nanaimo)
###############

# getwd()
# setwd("./LDP_ATS_Rescue")

# Install necessary packages if they are not yet installed
packages <- c("beepr", "dplyr", "lubridate","progress",
              "purrr","stringr", "tibble", "tictoc", "tidyverse", "tools", "Rcpp", "haven", "joyn")
install.packages(setdiff(packages, row.names(installed.packages())))

# Load necessary packages
{
  library(beepr)
  library(dplyr)
  library(lubridate)
  library(hms)
  library(purrr)
  library(stringr)
  library(tidyverse)
  library(tools)
  library(Rcpp) 
  library(haven) # read SAS files
  library(joyn)
}

###############

# Create a vector to hold the path to input and output files
working_directory <- "./TRAWL_BIOSAMPLE/02_intermediate_out/"
error_directory <- "./TRAWL_BIOSAMPLE/03_errors_out/"
final_directory <- "./TRAWL_BIOSAMPLE/04_final_output/"

################################  Step 1  #####################################
################### Read the CSV files and clean data type #########################

## Files that I do not specify the format do not exist in both formats
# read csv from sas files
{
  Trawl_77     <- read.csv(paste0(working_directory, "trawl77_SAS.csv"))
  Trawl_78     <- read.csv(paste0(working_directory, "trawl78_SAS.csv"))
  Trawl_79     <- read.csv(paste0(working_directory, "trawl79_SAS.csv"))
  Trawl_80     <- read.csv(paste0(working_directory, "trawl80_SAS.csv"))
  Trawl_81     <- read.csv(paste0(working_directory, "trawl81_SAS.csv"))
  Trawl_82     <- read.csv(paste0(working_directory, "trawl82_SAS.csv"))
  Trawl_83     <- read.csv(paste0(working_directory, "trawl83_SAS.csv"))
  Trawl_84_sas <- read.csv(paste0(working_directory, "trawl84_SAS.csv"))
  Trawl_88_sas <- read.csv(paste0(working_directory, "trawl88_SAS.csv"))
  Trawl_89_sas <- read.csv(paste0(working_directory, "trawl89_SAS.csv"))
  Trawl_90_sas <- read.csv(paste0(working_directory, "trawl90_SAS.csv"))
  Trawl_91_sas <- read.csv(paste0(working_directory, "trawl91_SAS.csv"))
  Trawl_92_sas <- read.csv(paste0(working_directory, "trawl92_SAS.csv"))
  Trawl_93_sas <- read.csv(paste0(working_directory, "trawl93_SAS.csv"))
  Trawl_95_sas <- read.csv(paste0(working_directory, "trawl95_SAS.csv"))
  Trawl_96_sas <- read.csv(paste0(working_directory, "trawl96_SAS.csv"))
}
# read csv from dat files
{
  Trawl_84_dat <- read.csv(paste0(working_directory, "trawl84_DAT.csv"))
  Trawl_85     <- read.csv(paste0(working_directory, "trawl85_DAT.csv"))
  Trawl_86     <- read.csv(paste0(working_directory, "trawl86_DAT.csv"))
  Trawl_87     <- read.csv(paste0(working_directory, "trawl87_DAT.csv"))
  Trawl_88_dat <- read.csv(paste0(working_directory, "trawl88_DAT.csv"))
  Trawl_89_dat <- read.csv(paste0(working_directory, "trawl89_DAT.csv"))
  Trawl_90_dat <- read.csv(paste0(working_directory, "trawl90_DAT.csv"))
  Trawl_91_dat <- read.csv(paste0(working_directory, "TRAWL91_DAT.csv"))
  Trawl_92_dat <- read.csv(paste0(working_directory, "TRAWL92_DAT.csv"))
  Trawl_93_dat <- read.csv(paste0(working_directory, "TRAWL93_DAT.csv"))
  Trawl_94     <- read.csv(paste0(working_directory, "TRAWL94_DAT.csv"))
  Trawl_95_dat <- read.csv(paste0(working_directory, "TRAWL95_DAT.csv"))
  Trawl_96_dat <- read.csv(paste0(working_directory, "TRAWL96_DAT.csv"))
  Trawl_97     <- read.csv(paste0(working_directory, "TRAWL97_DAT.csv"))
  Trawl_98     <- read.csv(paste0(working_directory, "TRAWL98_DAT.csv"))
  Trawl_99     <- read.csv(paste0(working_directory, "TRAWL99_DAT.csv"))
}

# check head names to combine them properly
names(Trawl_84_sas)

# Check if they have the same class
summary(Trawl_84_dat)
summary(Trawl_84_sas)

sapply(Trawl_84_sas[names(Trawl_84_sas)], class)
sapply(Trawl_84_dat[names(Trawl_84_dat)], class)

### make sure the types of data match
{
#Trawl_88_sas <- Trawl_88_sas %>%
#  mutate(processor = as.character(processor))
Trawl_88_dat <- Trawl_88_dat %>%
  mutate(scale_book = as.character(scale_book))

#Trawl_89_sas <- Trawl_89_sas %>%
#  mutate(processor = as.character(processor))
Trawl_89_dat <- Trawl_89_dat %>%
  mutate(scale_book = as.character(scale_book))

#Trawl_90_sas <- Trawl_90_sas %>%
#  mutate(processor = as.character(processor))
Trawl_90_dat <- Trawl_90_dat %>%
  mutate(scale_book = as.character(scale_book))

#Trawl_91_sas <- Trawl_91_sas %>%
#  mutate(processor = as.character(processor))
Trawl_91_dat <- Trawl_91_dat %>%
  mutate(scale_book = as.character(scale_book))

#Trawl_92_sas <- Trawl_92_sas %>%
#  mutate(processor = as.character(processor))
Trawl_92_dat <- Trawl_92_dat %>%
  mutate(scale_book = as.character(scale_book))

#Trawl_93_sas <- Trawl_93_sas %>%
#  mutate(processor = as.character(processor))
#Trawl_93_sas <- Trawl_93_sas %>%
#  mutate(duration_mi = as.character(duration_mi))
Trawl_93_dat <- Trawl_93_dat %>%
  mutate(scale_book = as.character(scale_book))

#Trawl_95_sas <- Trawl_95_sas %>%
#  mutate(processor = as.character(processor))
Trawl_95_dat <- Trawl_95_dat %>%
  mutate(scale_book = as.character(scale_book))

#Trawl_96_sas <- Trawl_96_sas %>%
#  mutate(processor = as.character(processor))
Trawl_96_dat <- Trawl_96_dat %>%
  mutate(scale_book = as.character(scale_book))
}

################################  Step 2  #####################################
################### full join to keep all columns and rows #########################

## full join to keep all columns and rows, we can clean them later

# combining several columns
Trawl_84 <- joyn::full_join(Trawl_84_dat, Trawl_84_sas, suffix = c(".dat", ".sas"),
                            by = c("process_date", "trawl_date", "fish_total",
                                   "fish_length_mm", "trawl_unique_ID", "fish_unique_ID", "species_code",
                                   "trawl_number", "processor", "lake_code", "lake_name",
                                   "scale_book", "scale", "age",
                                   "preservative_code", "duration_mi", "depth_m", "fish_id",
                                   "fish_description", "preservative_description", "weight_conversion_formula",
                                   "sample_number", "fish_weight_g", "aging_technique_name",
                                   "aging_technique", "trawl_month", "ats_year", "program_notes", "species_code_comment"))

# Combining Trawl of the following years
#Trawl_88 <- joyn::full_join(Trawl_88_sas, Trawl_88_dat, suffix = c(".sas", ".dat"),
Trawl_88 <- joyn::full_join(Trawl_88_dat, Trawl_88_sas, suffix = c(".dat", ".sas"),
                            by = c("process_date", "trawl_date", "fish_total",
                                   "fish_length_mm", "trawl_unique_ID", "fish_unique_ID", "species_code",
                                   "trawl_number", "processor", "lake_code", "lake_name",
                                   "scale_book", "scale", "age",
                                   "preservative_code", "duration_mi", "depth_m", "fish_id",
                                   "fish_description", "preservative_description", "weight_conversion_formula",
                                   "sample_number", "fish_weight_g", "aging_technique_name",
                                   "aging_technique", "trawl_month", "ats_year", "program_notes", "species_code_comment"))

#Trawl_89 <- joyn::full_join(Trawl_89_sas, Trawl_89_dat,  suffix = c(".sas", ".dat"),
Trawl_89 <- joyn::full_join(Trawl_89_dat, Trawl_89_sas, suffix = c(".dat", ".sas"),
                      by = c("process_date", "trawl_date", "fish_total",
                             "fish_length_mm", "trawl_unique_ID", "fish_unique_ID", "species_code",
                             "trawl_number", "processor", "lake_code", "lake_name",
                             "scale_book", "scale", "age",
                             "preservative_code", "duration_mi", "depth_m", "fish_id",
                             "fish_description", "preservative_description", "weight_conversion_formula",
                             "sample_number", "fish_weight_g", "aging_technique_name",
                             "aging_technique", "trawl_month", "ats_year", "program_notes", "species_code_comment"))

#Trawl_90 <- joyn::full_join(Trawl_90_sas, Trawl_90_dat, suffix = c(".sas", ".dat"),
Trawl_90 <- joyn::full_join(Trawl_90_dat, Trawl_90_sas, suffix = c(".dat", ".sas"),
                      by = c("process_date", "trawl_date", "fish_total",
                             "fish_length_mm", "trawl_unique_ID", "fish_unique_ID", "species_code",
                             "trawl_number", "processor", "lake_code", "lake_name",
                             "scale_book", "scale", "age",
                             "preservative_code", "duration_mi", "depth_m", "fish_id",
                             "fish_description", "preservative_description", "weight_conversion_formula",
                             "sample_number", "fish_weight_g", "aging_technique_name",
                             "aging_technique", "trawl_month", "ats_year", "program_notes", "species_code_comment"))

#Trawl_91 <- joyn::full_join(Trawl_91_sas, Trawl_91_dat,  suffix = c(".sas", ".dat"),
Trawl_91 <- joyn::full_join(Trawl_91_dat, Trawl_91_sas, suffix = c(".dat", ".sas"),
                      by = c("process_date", "trawl_date", "fish_total",
                             "fish_length_mm", "trawl_unique_ID", "fish_unique_ID", "species_code",
                             "trawl_number", "processor", "lake_code", "lake_name",
                             "scale_book", "scale", "age",
                             "preservative_code", "duration_mi", "depth_m", "fish_id",
                             "fish_description", "preservative_description", "weight_conversion_formula",
                             "sample_number", "fish_weight_g", "aging_technique_name",
                             "aging_technique", "trawl_month", "ats_year", "program_notes", "species_code_comment"))

#Trawl_92 <- joyn::full_join(Trawl_92_sas, Trawl_92_dat, suffix = c(".sas", ".dat"),
Trawl_92 <- joyn::full_join(Trawl_92_dat, Trawl_92_sas, suffix = c(".dat", ".sas"),
                      by = c("process_date", "trawl_date", "fish_total",
                             "fish_length_mm", "trawl_unique_ID", "fish_unique_ID", "species_code",
                             "trawl_number", "processor", "lake_code", "lake_name",
                             "scale_book", "scale", "age",
                             "preservative_code", "duration_mi", "depth_m", "fish_id",
                             "fish_description", "preservative_description", "weight_conversion_formula",
                             "sample_number", "fish_weight_g", "aging_technique_name",
                             "aging_technique", "trawl_month", "ats_year", "program_notes", "species_code_comment"))

#Trawl_93 <- joyn::full_join(Trawl_93_sas, Trawl_93_dat,  suffix = c(".sas", ".dat"),
Trawl_93 <- joyn::full_join(Trawl_93_dat, Trawl_93_sas, suffix = c(".dat", ".sas"),
                      by = c("process_date", "trawl_date", "fish_total",
                             "fish_length_mm", "trawl_unique_ID", "fish_unique_ID", "species_code",
                             "trawl_number", "processor", "lake_code", "lake_name",
                             "scale_book", "scale", "age",
                             "preservative_code", "duration_mi", "depth_m", "fish_id",
                             "fish_description", "preservative_description", "weight_conversion_formula",
                             "sample_number", "fish_weight_g", "aging_technique_name",
                             "aging_technique", "trawl_month", "ats_year", "program_notes", "species_code_comment"))

# Remove duplicates
Trawl_95_dat <- Trawl_95_dat[!duplicated(Trawl_95_dat$fish_unique_ID), ]
#Trawl_95_rows <- joyn::full_join(Trawl_95_sas, Trawl_95_dat,  suffix = c(".sas", ".dat"),
#                                 by = "fish_unique_ID", update_values = TRUE)


#Trawl_95 <- joyn::full_join(Trawl_95_sas, Trawl_95_dat,  suffix = c(".sas", ".dat"),
Trawl_95 <- joyn::full_join(Trawl_95_dat, Trawl_95_sas, suffix = c(".dat", ".sas"),
                      by = c("process_date", "trawl_date", "fish_total",
                             "fish_length_mm", "trawl_unique_ID", "fish_unique_ID", "species_code",
                             "trawl_number", "processor", "lake_code", "lake_name",
                             "start_time", "end_time", "scale_book", "scale", "age",
                             "preservative_code", "duration_mi", "depth_m", "fish_id",
                             "fish_description", "preservative_description", "weight_conversion_formula",
                             "sample_number", "fish_weight_g", "aging_technique_name",
                             "aging_technique", "trawl_month", "ats_year", "program_notes", "species_code_comment"))

#Trawl_96 <- joyn::full_join(Trawl_96_sas, Trawl_96_dat,  suffix = c(".sas", ".dat"),
Trawl_96 <- joyn::full_join(Trawl_96_dat, Trawl_96_sas, suffix = c(".dat", ".sas"),
                      by = c("process_date", "trawl_date", "fish_total",
                             "fish_length_mm", "trawl_unique_ID", "fish_unique_ID", "species_code",
                             "trawl_number", "processor", "lake_code", "lake_name",
                             "scale_book", "scale", "age",
                             "preservative_code", "duration_mi", "depth_m", "fish_id",
                             "fish_description", "preservative_description", "weight_conversion_formula",
                             "sample_number", "fish_weight_g", "aging_technique_name",
                             "aging_technique", "trawl_month", "ats_year", "program_notes", "species_code_comment"))


# Identify vectors to remove all unnecessary vectors using a pattern
vectors_to_remove <- c(ls(pattern = "_dat$"),ls(pattern = "_sas$")) 
# Remove the identified vectors
rm(list = vectors_to_remove)

################################  Step 3  #####################################
########### correct data type between the different data frames ################

# Change data type for depth_m_comment
{
  Trawl_85 <- Trawl_85 %>%
    mutate(depth_m_comment = as.character(depth_m_comment))
  Trawl_86 <- Trawl_86 %>%
    mutate(depth_m_comment = as.character(depth_m_comment))
  Trawl_94 <- Trawl_94 %>%
    mutate(depth_m_comment = as.character(depth_m_comment))
  Trawl_97 <- Trawl_97 %>%
    mutate(depth_m_comment = as.character(depth_m_comment))
  Trawl_98 <- Trawl_98 %>%
    mutate(depth_m_comment = as.character(depth_m_comment))
  Trawl_99 <- Trawl_99 %>%
    mutate(depth_m_comment = as.character(depth_m_comment))
}

# Change data type for trawl_number_comment
Trawl_91 <- Trawl_91 %>%
  mutate(trawl_number_comment = as.character(trawl_number_comment))

# Join all data frames
ls()
df_trawl_list <- mget(ls(pattern = "^Trawl_\\d+$"))

## Change the data types so they all match 
# Function to change the data type of a column
change_column_type <- function(data, column_name, new_type) {
  data %>%
    mutate(!!sym(column_name) := as(!!sym(column_name), new_type))
}

# Apply the function to all data frames in the list for duration_mi
df_trawl_list <- map(df_trawl_list, ~change_column_type(.x, "duration_mi", "character"))
# processor
df_trawl_list <- map(df_trawl_list, ~change_column_type(.x, "processor", "character"))
# scale_book
df_trawl_list <- map(df_trawl_list, ~change_column_type(.x, "scale_book", "character"))
# sample_number
df_trawl_list <- map(df_trawl_list, ~change_column_type(.x, "sample_number", "character"))

################################  Step 4  #####################################
################### combine all data frames into one #########################

# Combine the data frames
df_joined <- bind_rows(df_trawl_list)

# Identify vectors to remove all unnecessary vectors using a pattern
vectors_to_remove <- c(ls(pattern = "^Trawl_")) 
# Remove the identified vectors
rm(list = vectors_to_remove)

################################  Step 5  #####################################
################### Cleaning the columns and combining rows #########################
# Save raw combined column
write.csv(df_joined, paste0(working_directory, "/combined_raw_df_trawl.csv"), row.names = FALSE)

# Check the table 
df_joined_dup <- df_joined[duplicated(df_joined$fish_unique_ID), ]
summary(df_joined)

# Flag errors with fish total number in SAS files
df_sas_total_fish_error <- df_joined %>%
  group_by(fish_unique_ID) %>%
  filter(n() > 1) %>%
  distinct(fish_total, .keep_all = TRUE) %>%
  filter(n() > 1) %>%
  arrange(fish_unique_ID) %>%
  ungroup()

# Save the errors in the fish_total
write.csv(df_sas_total_fish_error, paste0(error_directory, "/sas_total_fish_errors.csv"), row.names = FALSE)

# Correct the total number of fish when they differ between source-SAS and source-DAT files
# group by unique ID to display the putative duplicates.
df_joined <- df_joined %>%
  group_by(fish_unique_ID) %>%
  mutate(
    fish_total_updated = fish_total[which(!is.na(source_file.dat))][1],
    # Update total number of fish in the source-SAS rows only if it is not NA in both rows.
    fish_total = if_else(!is.na(source_file.sas) & any(is.na(source_file.dat)), fish_total_updated, fish_total)
  ) %>%
  select(-fish_total_updated)  %>%
  ungroup()

# Collapse duplicated rows into one row, and keep .dat row values if conflict appears
# Function to resolve the conflicts/similarities among the columns
resolve_column <- function(values, source) {
  # Extract unique non-NA values
  non_na_values <- unique(values[!is.na(values)])
  
  # All values are NA
  if (length(non_na_values) == 0) {
    return(list(value = NA, status = "No change"))
  }
  
  #  Only one unique non-NA value, NA fill
  if (length(non_na_values) == 1) {
    return(list(
      value  = non_na_values[1],
      status = ifelse(any(is.na(values)), "NA updates", "No change")
    ))
  }
  # in case of conflict, prefer .dat source (ignore NA)
  has_dat <- any(endsWith(source, ".dat"), na.rm = TRUE)
  
  if (has_dat) {
    dat_value <- values[
      !is.na(source) &
        endsWith(source, ".dat") &
        !is.na(values)
    ][1]
    return(list(value = dat_value, status = "Conflict within"))
  }
  # Conflict but no info from .dat files
  list(value = values[1], status = "No change")
}

## Collapse the data and create a column to flag the changes
df_final_no_dup <- df_joined %>%
  group_by(fish_unique_ID, fish_total) %>%
  summarise(
    # Keep source column explicitly
    
    source_file.dat2 = first(source_file.dat),

    # Compute flags and add to a column called "merging_update_type"
    merging_update_type = {
      # Single-row group
      if (n() == 1) {
        "Single-row"
      } else {
        flags <- map_chr(
          #cur_data() %>% select(-source_file.dat), # this deletes the source_file.dat column 

          pick(-source_file.dat2),
          ~ resolve_column(.x, source_file.dat2)$status

        )
        # State priorities
        if ("Conflict within" %in% flags) {
          "Conflict within"
        } else if ("NA updates" %in% flags) {
          "NA updates"
        } else {
          "No change"
        }
      }
    },
    # Resolve all other columns
    across(

      -source_file.dat2,
      ~ resolve_column(.x, source_file.dat2)$value

    ),
  
    .groups = "drop"
  )

### Combine source_file, source_file.sas and source_file.dat into a new 'source_files' column
df_final <- df_final_no_dup %>%
  unite(col = "source_files", source_file, source_file.sas, source_file.dat, sep = ", ") %>%
  select(-source_file.dat2)

# Remove NAs that were added to the rows
replacement_pattern <- c("^NA, " = "",
                         "NA, " = "",
                         ", NA, NA" = "",
                         ", NA" = "")
df_final$source_files <- str_replace_all(df_final$source_files, replacement_pattern)

### Clean typos in the sample_type column
replacement_pattern <- c("TRAWLL" = "Trawl",
                         "TTRAWL" = "Trawl")
df_final$sample_type <- str_replace_all(df_final$sample_type, replacement_pattern)

# Reorganize columns
df_final <- df_final %>% 
  relocate(trawl_date, "trawl_comment" = trawl_location, lake_code, lake_name, process_date, processor,
           start_time, end_time, start_time.sas, end_time.sas, start_time.dat, end_time.dat, duration_mi, depth_m,  
           trawl_number, sample_type, species_code, fish_description, species_code_comment, fish_length_mm, fish_weight_g, 
           weight_conversion_formula, standardized_weight_g, fish_total, fish_id, 
           preservative_code, preservative_description, sample_number, scale, scale_book,scale_book_letter, age, 
           aging_technique, aging_technique_name, source_files, source_line, trawl_unique_ID, fish_unique_ID, 
           trawl_month, ats_year, comment,
           time_comment, preservative_code_comment, depth_m_comment, trawl_date_comment, trawl_number_comment, 
           program_notes,  merging_update_type, .joyn)

# Are there still duplicates? There are some rows with equal fish_unique_id but are not the same record
all_duplicates <- df_final %>%
  group_by(fish_unique_ID) %>%
  filter(n() > 1) %>%
  ungroup()

# Save duplicated rows until here
write.csv(all_duplicates, paste0(error_directory, "/duplicated_df_trawl.csv"), row.names = FALSE)

# Flag duplicates in the final dataset
df_final <- df_final %>%
  mutate(duplicate_flag =
           if_else(duplicated(across(fish_unique_ID)) |
                     duplicated(across(fish_unique_ID), fromLast = TRUE),
                   "Potential duplicate record",
                   NA_character_))

### Combine start_time.sas and .dat into a new 'start_time' column
# First standardize midnight times: instead of 24, represent it as 00
# Remove decimals from .sas time columns
df_final_chk_time <- df_final %>%
  mutate(across(c(start_time.dat, end_time.dat, start_time.sas, end_time.sas, start_time, end_time), 
                ~ str_replace_all(.x, pattern = "^24", replacement = "00"))) %>%
  mutate(across(c(start_time.sas, end_time.sas), 
                ~ str_replace_all(.x, pattern = ".\\d{5}$", replacement = "")))

## Before combining columns, flag the ones with errors and correct them
# Define the regex pattern for HH:MM:SS format
time_pattern <- "^([01]?[0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]$"

# Use grepl() to create a logical vector indicating valid formats in .dat time
is_valid_dat <- ifelse(is.na(df_final_chk_time$start_time.dat), NA, grepl(time_pattern, df_final_chk_time$start_time.dat))
is_valid_sas <- ifelse(is.na(df_final_chk_time$start_time.sas), NA, grepl(time_pattern, df_final_chk_time$start_time.sas))
is_valid <- ifelse(is.na(df_final_chk_time$start_time), NA, grepl(time_pattern, df_final_chk_time$start_time))

# Add a new column to the data frame to flag invalid entries
df_final_chk_time <- df_final_chk_time %>%
  mutate(invalid_start_time = case_when(is_valid_sas == "TRUE" ~ NA_character_,
                                        is_valid_dat == "TRUE" ~ NA_character_,
                                        is_valid == "TRUE" ~ NA_character_,
                                        is.na(is_valid_sas) & is.na(is_valid_dat) & is.na(is_valid) ~ NA_character_,
                                        TRUE ~ "Invalid format"))

# Check number of problematic rows
sum(df_final_chk_time$invalid_start_time == "Invalid format", na.rm = TRUE)

# Save errors in start and end time in separate document
start_time_errors <- df_final_chk_time[!is.na(df_final_chk_time$invalid_start_time) & df_final_chk_time$invalid_start_time == "Invalid format",]
write.csv(start_time_errors, paste0(error_directory, "/start_time_errors.csv"), row.names = FALSE)

## Flagging errors in the end time
# Use grepl() to create a logical vector indicating valid formats
is_valid_end_time_dat <- ifelse(is.na(df_final_chk_time$end_time.dat), NA, grepl(time_pattern, df_final_chk_time$end_time.dat))
is_valid_end_time_sas <- ifelse(is.na(df_final_chk_time$end_time.sas), NA, grepl(time_pattern, df_final_chk_time$end_time.sas))
is_valid_end_time <- ifelse(is.na(df_final_chk_time$end_time), NA, grepl(time_pattern, df_final_chk_time$end_time))

# Add a new column to the data frame to flag invalid entries
df_final_flag_time <- df_final_chk_time %>%
  mutate(invalid_end_time = case_when(is_valid_end_time_sas == "TRUE" ~ NA_character_,
                                      is_valid_end_time_dat == "TRUE" ~ NA_character_,
                                      is_valid_end_time == "TRUE" ~ NA_character_,
                                      is.na(is_valid_end_time_sas) & is.na(is_valid_end_time_dat) & is.na(is_valid_end_time) ~ NA_character_,
                                        TRUE ~ "Invalid format"))

# Check number of problematic rows
sum(df_final_flag_time$invalid_end_time == "Invalid format", na.rm = TRUE)

# Save errors in start and end time in separate document
end_time_errors <- df_final_flag_time[!is.na(df_final_flag_time$invalid_end_time) & df_final_flag_time$invalid_end_time == "Invalid format",]
write.csv(end_time_errors, paste0(error_directory, "/end_time_errors.csv"), row.names = FALSE)

# Manually fix some of the errors identified
#  everything with 0203 - fish_unique_ID == 1997-09-17_69_9_15_7_1_1.35_49
df_final_flag_time <- df_final_flag_time %>%
  mutate(
    end_time = ifelse(duration_mi == "0203", "02:03:00", end_time),
    time_comment = ifelse(duration_mi == "0203", "0203 error in duration_mi was end_time - corrected", time_comment),
    duration_mi = ifelse(duration_mi == "0203", "16", duration_mi)
  )

# Summary of start_time with wrong format    
df_final_flag_time %>%
  filter(invalid_start_time == "Invalid format") %>%
  group_by(start_time, start_time.sas, start_time.dat) %>%
  summarise(count = n()) -> summary_table_time

# Clean the invalid numbers. Replace them by NA.
# start_time column substitution patterns
replacement_pattern <- c("08:65:00" = NA_character_,
                         "14:69:00" = NA_character_,
                         "15:92:00" = NA_character_,
                         "21:89:00" = NA_character_)
df_final_raw <- df_final_flag_time %>%
  mutate(start_time = str_replace_all(start_time, replacement_pattern)) 

# start_time.sas column substitution patterns
replacement_pattern <- c("99:40:13" = NA_character_, "100:38:24" = NA_character_)
df_final_raw <- df_final_raw %>%
  mutate(start_time.sas = str_replace_all(start_time.sas, replacement_pattern)) 

# start_time.dat column substitution patterns
replacement_pattern <- c("99:99:00" = NA_character_, "00:0\\?:00" = NA_character_)
df_final_raw <- df_final_raw %>%
  mutate(start_time.dat = str_replace_all(start_time.dat, replacement_pattern)) 

### Combine start_time and end_time columns
# First combine start_time.dat and start_time.sas, prioritizing values for the .dat column when both are present
# Then combine everything in the start_time column and delete unnecessary columns
df_final_raw$start_time_combined <- coalesce(df_final_raw$start_time.dat, df_final_raw$start_time.sas)
df_final_raw$start_time_combined <- trimws(df_final_raw$start_time_combined)
df_final_raw$start_time <- coalesce(df_final_raw$start_time, df_final_raw$start_time_combined)
df_final_raw <- df_final_raw %>%
  select(-start_time.dat, -start_time.sas, -start_time_combined) 

# First clean invalid format. Combine end_time.dat and end_time.sas, prioritizing values for the .dat column when both are present
# Then combine everything in the end_time column and delete unnecessary columns
replacement_pattern <- c("NA:NA:00" = NA_character_)
df_final_raw <- df_final_raw %>%
  mutate(end_time = str_replace_all(end_time, replacement_pattern)) 

df_final_raw$end_time_combined <- coalesce(df_final_raw$end_time.dat, df_final_raw$end_time.sas)
df_final_raw$end_time_combined <- trimws(df_final_raw$end_time_combined)
df_final_raw$end_time <- coalesce(df_final_raw$end_time, df_final_raw$end_time_combined)
df_final_raw <- df_final_raw %>%
  select(-end_time.dat, -end_time.sas, -end_time_combined) 

### Separate columns with fish descriptions and common name
# Cleaning the columns
df_final_clean_time <- df_final_raw %>%
  # Fish description column removing comments and placing into separate column 
  mutate(
    ### extract the description at the end
    fish_description_clean = str_extract(fish_description, "\\s\\([:graph:]+\\)$"),
    
    ### extract the rest of the string as the common name of the fish
    fish_description = str_trim(str_remove(fish_description, "\\s\\([:graph:]+\\)$")),
    fish_description = case_when(species_code_comment == "KOKANEE" ~ "Kokanee",
                                 TRUE ~ fish_description),
    
    ### Save the stage description in the fish_description column
    fish_name = as.character(fish_description_clean)
    ) %>%
  unite(species_code_comment, species_code_comment, fish_name, sep = ",", na.rm = TRUE) %>%
  ### extract the description at the end
  select(-fish_description_clean)

# Standardize comments in the species_code_comment column
replacement_pattern <- c("\\(COHO FRY\\), \\(Fry\\)" = "Fry",
                         "\\(COHO SMOLT\\), \\(Smolt\\)" = "Smolt",
                         "COHO \\(SMLT\\), \\(Smolt\\)" = "Smolt",
                         "COHO \\(SMLT\\), Smolt"  = "Smolt",
                         "\\(PEAMOUTH CHUB\\)" = "",
                         "\\(SOCKEYE FRY\\), \\(Fry\\)" = "Fry",
                         "\\(SOCKEYE FRY\\), Fry" = "Fry",
                         "SOCKEYE  \\(JUV\\), \\(Juvenile\\)" = "Juvenile",
                         "SOCKEYE \\(JUV\\), \\(Juvenile\\)" = "Juvenile",
                         "SOCKEYE FRY, \\(Juvenile\\)" = "Juvenile",
                         "SOCKEYE JUVENILE, \\(Juvenile\\)" = "Juvenile",
                         "SOCKEYE, \\(Juvenile\\)" = "Juvenile",
                         "\\(SOCKEYE\\), Juvenile" = "Juvenile",
                         "SOCKEYE  \\(JUV\\), Juvenile" = "Juvenile",
                         "SOCKEYE \\(JUV\\), Juvenile" = "Juvenile",
                         "SOCKEYE JUVENILE, Juvenile" = "Juvenile",
                         "SOCKEYE, Juvenile" = "Juvenile",
                         "SUCKER \\(CATOSTOMUS SP.\\), \\(Catostomus\\)" = "",
                         "WHITEFISH \\(COREGONUS SP.\\), \\(Coregonus\\)" = "",
                         "REDSIDED SHINER\\(RICHARDSONIUS SP.\\)" = "")

df_final_clean_time <- df_final_clean_time %>%
  mutate(species_code_comment = str_replace_all(species_code_comment, replacement_pattern))

replacement_pattern <- c("SOCKEYE \\(1\\+\\), \\(1\\+\\)" = "1+",
                         "SOCKEYE \\(2\\+\\), \\(2\\+\\)" = "2+",
                         "SOCKEYE \\(FRY\\), \\(Fry\\)" = "Fry",
                         "SOCKEYE, \\(Fry\\)" = "Fry",
                         "FRY, \\(Juvenile\\)" = "Fry",
                         "SOCKEYE FRY, \\(Fry\\)" = "Fry",
                         "SOCKEYE Fry" = "Fry",
                         "\\(SOCKEYE\\), \\(Juvenile\\)" = "Juvenile",
                         "\\(2 STICKLEBACK ADULTS\\)" = "Adult",
                         "\\(ADULT STICKLE\\)" = "Adult",
                         "\\(STICKLE ADULTS\\)" = "Adult",
                         "\\(STICKLEBACK FRY\\), Fry" = "Fry",
                         "\\(STICKLEBACK SUBADULTS\\), \\(Sub-adult\\)" = "Subadult",
                         "\\(STICKLEBACK SUBADULTSS\\), \\(Sub-adult\\)" = "Subadult",
                         "\\(SUBADULT STICKLES\\), \\(Sub\\-adult\\)" = "Subadult",
                         "ADULT FEMALES" = "Adult female",
                         "STICKLEBACK \\(SUBADULT\\), \\(Sub-adult\\)" = "Subadult",
                         "STICKLEBACK SUBADULTS, \\(Sub-adult\\)" = "Subadult",
                         "STICKLEBACK SUBADULTSS, \\(Sub-adult\\)" = "Subadult")

df_final_clean_time <- df_final_clean_time %>%
  mutate(species_code_comment = str_replace_all(species_code_comment, replacement_pattern))

replacement_pattern <- c("GRAVID FEMALES" = "Gravid female",
                         "\\(STICKLEBACK FRY\\), \\(Fry\\)" = "Fry",
                         "COHO \\(FRY\\), \\(Fry\\)" = "Fry",
                         "STICKLE ADULTS" = "Adult",
                         "STICKLEBACK ADULTS" = "Adult",
                         "STICKLEBACK FRY, Fry" = "Fry",
                         "STICKLEBACK FRY, \\(Fry\\)" = "Fry",
                         "STICKLEBACK GRAVID FEMALE" =  "Gravid female",
                         "STICKLEBACK, \\(Sub-adult\\)" = "Subadult",
                         "STICKLEBACK, Fry" = "Fry",
                         "SOCKEYE FRY, Fry" = "Fry",
                         "SOCKEYE FRY, \\(Fry\\)" = "Fry",
                         "STICKLEBACK, \\(Fry\\)" = "Fry",
                         "SOCKEYE, Fry" = "Fry",
                         "STICKLEBACK, \\(Sub-adult\\)" = "Subadult")

df_final_clean_time <- df_final_clean_time %>%
  mutate(species_code_comment = str_replace_all(species_code_comment, replacement_pattern))

replacement_pattern <- c(", \\(Adult\\)" = "Adult",
                         ", \\(Juvenile\\)" = "Juvenile",
                         "\\(STICKLE FRY\\)" = "Fry",
                         "\\(STICKLE SUBADULTS\\)" = "Subadult",
                         "\\(STICKLE SUBADULT\\)" = "Subadult",
                         "\\(STICKLEBACK\\)" = "",
                         "\\(Sub-adult\\)" = "Subadult",
                         "GRAVID FEMALE" =  "Gravid female",
                         "STICKLE ADULT" = "Adult",
                         "SUB ADULTS" = "Subadult")

df_final_clean_time <- df_final_clean_time %>%
  mutate(species_code_comment = str_replace_all(species_code_comment, replacement_pattern))

replacement_pattern <- c("\\(STICKLES\\)" = "",
                         "\\(STICKLE\\)" = "",
                         "ADULTS" = "Adult",
                         "ADULT" = "Adult",
                         "STICKLE" = "",
                         " \\(Fry\\)" = "Fry",
                         " Subadult" = "Subadult",
                         " \\(Juvenile\\)" = "Juvenile",
                         "FRY" = "Fry",
                         " \\(Smolt\\)" = "Smolt",
                         "CHINOOK" = "",
                         "DOLLY VARDEN" = "",
                         "KOKANEE" = "",
                         "LAMPREY" = "",
                         "PEAMOUTH CHUB" = "",
                         "PINKS" = "",
                         "SCULPIN" = "",
                         "BACK" = "")

df_final_clean_time <- df_final_clean_time %>%
  mutate(species_code_comment = str_replace_all(species_code_comment, replacement_pattern))

### Create Look up table to correct lakes and fish names
# add a column with fish scientific names: genus and species
fish_scientific_name_lookup_table <- read.csv("./TRAWL_BIOSAMPLE/00_raw_data/03_AA_look_up_tables/AA_fish_scientific_name_lookup.csv")

# Add a comment for Dolly Varden - taxonomic issues 
df_final_clean_time$comment <- ifelse(df_final_clean_time$fish_description == "Dolly Varden", 
                                      "Salvelinus malma and S. confluentus might have unsolved taxonomic issues", 
                                      df_final_clean_time$comment)

# Remove abbreviations in the name of the lakes by importing the lookup table
lake_name <- read.csv("./TRAWL_BIOSAMPLE/00_raw_data/04_YS_look_up_tables/lake_codes.csv")

# Join the tables
df_final_clean_time <- df_final_clean_time %>%
  rename("species_info_code" = species_code) %>%
  dplyr::left_join(fish_scientific_name_lookup_table, by = "species_info_code") %>%
  select(-lake_name) %>%
  mutate(comment = ifelse(lake_code == 124, 
                          ifelse(is.na(comment) | comment == "",
                                 "Changed lake code from No Name Lake to Link Lake",
                                 paste("Changed lake code from No Name Lake to Link Lake", comment, sep = "; ")),
                          comment),
         lake_code = case_when(lake_code == 124 ~ 180,
                               TRUE ~ lake_code)) %>%
  dplyr::left_join(lake_name, by = "lake_code") %>%
  mutate(age = ifelse(is.na(age) & !is.na(age_class), age_class, age)) %>%
  mutate(species_code_comment = ifelse(species_code_comment == "Juvenile", life_stage, species_code_comment)) %>%
  select(-species_common_name, -age_class, -life_stage)
  
df_final_clean_time %>%
  group_by(fish_description, species_code_comment, age, genus_name, species_name) %>%
  summarise(count = n()) -> summary_table

# Save error table with the rows with empty names for fish species.
no_species_record_rows <- df_final_clean_time[is.na(df_final_clean_time$fish_description),]
write.csv(no_species_record_rows, paste0(error_directory, "/no_species_record_rows.csv"), row.names = FALSE)

# Flag rows without species records
df_final_clean_species <- df_final_clean_time %>%
  mutate(no_species_name_comments = case_when(species_info_code == 99 & is.na(fish_length_mm) & is.na(fish_weight_g) ~ 
                                                "Sampling effort documented, but no fish catch recorded",
                                              species_info_code == 99 & !is.na(fish_length_mm) & !is.na(fish_weight_g) ~ 
                                                "Fish recorded with missing species code",
                                              TRUE ~ NA_character_))

# Delete rows with no species info from the final matrix
#df_final_clean_species <- df_final_clean_species[!is.na(df_final_clean_species$fish_description), ] # Uncomment if you'd like to delete them

#### Work on the duration_mi column. Remove "Min" from the column
df_final_clean_species <- df_final_clean_species %>%
  mutate(
    ### extract the digits at the start
    duration_mi_clean = str_extract(duration_mi, "^\\d+"),
    ### convert number to integer (optional, based on your needs)
    duration_mi = as.integer(duration_mi_clean)
  ) %>%
  select(-duration_mi_clean)

# Flag problematic rows on duration_mi
df_final_clean_species <- df_final_clean_species %>%
  mutate(invalid_duration_time = case_when(duration_mi < 60 ~ NA_character_,
                                        TRUE ~ "Greater than 60 min, likely error"))
# Check number of problematic rows
sum(df_final_clean_species$invalid_duration_time == "Greater than 60 min, likely error", na.rm = TRUE)

# Save errors in duration in separate document
duration_mi_errors <- df_final_clean_species[df_final_clean_species$invalid_duration_time == "Greater than 60 min, likely error",]
write.csv(duration_mi_errors, paste0(error_directory, "/duration_mi_errors.csv"), row.names = FALSE)

## In the duration_mi columns, correct values "365" and "535" 
df_final_clean_species <- df_final_clean_species %>%
  mutate(duration_mi = as.character(duration_mi)) %>%
  mutate(duration_mi = case_when(duration_mi == "365" ~ NA_character_,
                                 duration_mi == "535" ~ NA_character_,
                                 TRUE ~ duration_mi))
df_final_clean_species$duration_mi <- as.integer(df_final_clean_species$duration_mi)

# Replace all "99" and "999" by NA across all columns
df_final_clean_species <- df_final_clean_species %>%
  mutate(across(c(duration_mi, processor, depth_m, scale, scale_book, trawl_number), ~ str_replace_all(.x, pattern = "999", replacement = NA_character_))) %>%
  mutate(across(c(duration_mi, processor, depth_m, trawl_number), ~ str_replace_all(.x, pattern = "99", replacement = NA_character_))) %>%
  mutate(duration_mi = as.integer(duration_mi)) 

## Calculate duration_mi from the difference of the start_time and end_time columns
df_final_calc_duration <- df_final_clean_species %>%
  mutate(across(c(start_time, end_time), 
                ~ str_replace_all(.x, pattern = "^00", replacement = "24")))
  
# Make sure all rows have the same format
df_final_calc_duration <- df_final_calc_duration %>%
  mutate(start_t = hms(start_time),
         end_t  = hms(end_time)) %>%
  mutate(across(c(start_time, end_time), 
                ~ str_replace_all(.x, pattern = "^24", replacement = "00")))

# Create a column with the calculated duration in minutes
df_final_calc_duration <- df_final_calc_duration %>%
  mutate(calc_duration_time = if_else(!is.na(start_t) & !is.na(end_t), 
                                      if_else(as.numeric(end_t) < as.numeric(start_t),
                                              (as.numeric(end_t) + 24*60*60 - as.numeric(start_t)) / 60,
                                              (as.numeric(end_t) - as.numeric(start_t)) / 60), NA_real_)) %>%
  mutate(calc_duration_time = round(calc_duration_time, 1))

# Final duration column
df_final_calc_duration <- df_final_calc_duration %>%
  mutate(duration_final = case_when(!is.na(duration_mi) ~ duration_mi,
                                    is.na(duration_mi) & !is.na(calc_duration_time) ~ calc_duration_time,
                                    TRUE ~ NA_real_),
    duration_comment = case_when(
      # duration exists and matches times
      !is.na(duration_mi) & !is.na(calc_duration_time) &
        abs(duration_mi - calc_duration_time) < 1 ~ "matches calculated start_time and end_time",
      # duration exists but does NOT match times
      !is.na(duration_mi) & !is.na(calc_duration_time) &
        abs(duration_mi - calc_duration_time) >= 1 ~ "did NOT match calculated start_time and end_time, likely end_time error",
      # duration calculated from times
      is.na(duration_mi) & !is.na(calc_duration_time) ~ "duration not provided, calculated from start_time, end_time",
      # nothing possible
      TRUE ~ "duration could not be calculated"))

# Check number of problematic rows
sum(df_final_calc_duration$duration_comment == "did NOT match calculated start_time and end_time, likely end_time error", na.rm = TRUE)
sum(df_final_calc_duration$duration_comment == "duration not provided, calculated from start_time, end_time", na.rm = TRUE)

# Save mismatches in duration in separate document
duration_mismatch <- df_final_calc_duration[!is.na(df_final_calc_duration$duration_comment) & df_final_calc_duration$duration_comment == "does NOT match calculated start_time and end_time, likely end_time error",]
write.csv(duration_mismatch, paste0(error_directory, "/duration_mismatch.csv"), row.names = FALSE)

# Correct start_time and end_time based on the calculated duration field.
# Read lookup table with the corrected values and specific comments
duration_mi_lookup_table_corrections <- read.csv("./TRAWL_BIOSAMPLE/00_raw_data/03_AA_look_up_tables/fix_trawl_times.csv")

# Use the lookup table to correct the values in the final dataset
df_final_calc_duration <- df_final_calc_duration %>%
  dplyr::left_join(duration_mi_lookup_table_corrections, by = c("trawl_unique_ID", "calc_duration_time")) %>%
  # Replacement commands for start_time and end_time columns
  mutate(start_time = case_when(start_time != new_start_time  ~ new_start_time,
                                    TRUE ~ start_time),
         end_time = case_when(end_time != new_end_time ~ new_end_time,
                              TRUE ~ end_time)) %>%
  # Flag changes in the comment column
  unite(duration_comment, duration_comment, comments_start_end_times, sep = "; ", na.rm = TRUE) %>%
  select(-new_start_time, -old_start_time, -old_end_time, -new_end_time, -start_t, -end_t)

# Include additional comments and corrections performed in duration_mi column
df_final_calc_duration <- df_final_calc_duration %>%
  mutate(duration_comment2 = ifelse(fish_unique_ID == "1986-08-30_3_1_0_2_1_4.11_65", 
                                    "Note: unusually long Duration, but data not changed", NA_character_)) %>%
  mutate(duration_comment2 = ifelse(duration_mi == 0 & start_time == "03:22:00",
                                    "duration_minutes changed from 0 to 15, based on start and end times", NA_character_)) %>%
  unite(duration_comment, duration_comment, duration_comment2, sep = "; ", na.rm = TRUE) %>%
  mutate(duration_mi = case_when(duration_mi == 0 & start_time == "03:22:00" ~ 15,
                                 TRUE ~ duration_mi))

# Re-calculate duration in minutes after correcting anomalous records
# Make sure all rows have the same format
df_final_calc_duration <- df_final_calc_duration %>%
  mutate(start_t = hms(start_time),
         end_t  = hms(end_time)) %>%
  mutate(across(c(start_time, end_time), 
                ~ str_replace_all(.x, pattern = "^24", replacement = "00")))

df_final_calc_duration <- df_final_calc_duration %>%
  mutate(calc_duration_time = if_else(!is.na(start_t) & !is.na(end_t), 
                                      if_else(as.numeric(end_t) < as.numeric(start_t),
                                              (as.numeric(end_t) + 24*60*60 - as.numeric(start_t)) / 60,
                                              (as.numeric(end_t) - as.numeric(start_t)) / 60), NA_real_)) %>%
  mutate(calc_duration_time = round(calc_duration_time, 1))


## calculate missing end_time values in anew column
df_final_calc_end <- df_final_calc_duration %>%
  mutate(calc_end_time = as.numeric(start_t) + as.numeric(duration_mi) * 60,
         calc_end_time_comment = case_when(!is.na(end_time) ~ NA_character_,
                                           is.na(calc_end_time) ~ "end_time could not be calculated",
                                          TRUE ~ "end_time missing, calculated from start_time + duration_mi")) %>%
  select(-start_t, -end_t)

# Convert seconds to hms format
seconds_to_hms <- function(x) {sprintf("%02d:%02d:%02d", x %/% 3600, (x %% 3600) %/% 60, x %% 60)} 
df_final_calc_end$calc_end_time <- seconds_to_hms(df_final_calc_end$calc_end_time)     
# Remove NA rows
df_final_calc_end <- df_final_calc_end %>%
  mutate(calc_end_time = ifelse(calc_end_time == "NA:NA:NA", NA, calc_end_time)) %>%
  mutate(calc_end_time = str_replace_all(calc_end_time, pattern = "^24", replacement = "00"))  %>%
  mutate(calc_end_time = str_replace_all(calc_end_time, pattern = "^25", replacement = "01"))

# Check number of calculated end time rows
sum(df_final_calc_end$calc_end_time_comment == "end_time missing, calculated from start_time + duration_mi", na.rm = TRUE)

df_final_calc_end %>%
  group_by(start_time, end_time, calc_end_time, calc_end_time_comment, duration_mi, calc_duration_time, duration_comment) %>%
  summarise(count = n()) -> summary_table

#### Correct errors in preservative_code column
code = c("971", "270", "350", "11", "35")
df_final_calc_end <- df_final_calc_end %>%
  mutate(preservative_code = as.character(preservative_code)) %>%
  mutate(preservative_code_comment = ifelse(preservative_code %in% code, 
                                            ifelse(is.na(preservative_code_comment) | preservative_code_comment == "",
                                                   "corrected typo error in preservative_code",
                                                   paste("corrected typo error in preservative_code", preservative_code_comment, sep = "; ")),
                                            preservative_code_comment)) %>%
  mutate(preservative_code = case_when(preservative_code == "98" ~ NA_character_,
                                       preservative_code == "971" ~ "97",
                                       preservative_code == "9" ~ NA_character_,
                                       preservative_code == "270" ~ "2",
                                       preservative_code == "350" ~ "3",
                                       preservative_code == "11" ~ "1",
                                       preservative_code == "35" ~ "3",
                                       TRUE ~ preservative_code)) %>%
  mutate(preservative_code = as.integer(preservative_code)) %>%
  # Clean the preservative_code_comment column, removing extra spaces and tabs from rows
  mutate(preservative_code_comment = str_squish(preservative_code_comment))

# Detect preservation comments in the trawl_comment column
ethanol_trawl_location <- grepl("ethanol", df_final_calc_end$trawl_comment, ignore.case = TRUE)
sum(ethanol_trawl_location == "TRUE", na.rm = TRUE)

df_final_calc_end %>%
  filter(ethanol_trawl_location) %>%
  group_by(fish_unique_ID, trawl_comment, preservative_code, preservative_description, preservative_code_comment) %>%
  summarise(count = n()) -> summary_table

# Replace preservative_description by the info from trawl_comment
df_final_calc_end <- df_final_calc_end %>%
  mutate(preservative_description = ifelse(ethanol_trawl_location =="TRUE", "95% Ethanol", preservative_description),
         preservative_code = ifelse(ethanol_trawl_location == "TRUE", 5, preservative_code),
         preservative_code_comment = if_else(ethanol_trawl_location == "TRUE",
                                             if_else(is.na(preservative_code_comment) | preservative_code_comment == "",
                                                     "Preservative description provided in trawl_comment column", 
                                                     paste(preservative_code_comment, "Preservative description provided in trawl_comment column", 
                                                           sep = "; ")), preservative_code_comment))

# Fill up the missing values using a lookup table
preservative_code_lookup_table <- read.csv("./TRAWL_BIOSAMPLE/00_raw_data/03_AA_look_up_tables/AA_DFO_preservative_code_lookup_table.csv")
#df_final_calc_end <- rows_patch(df_final_calc_end, preservative_code_lookup_table, by = "preservative_code", unmatched = "ignore")

df_final_calc_end <- df_final_calc_end %>%
  select(-preservative_description, -weight_conversion_formula) %>%
  dplyr::left_join(preservative_code_lookup_table, by = "preservative_code")

# Check if preservative_description match preservative_code
df_final_calc_end %>%
  group_by(preservative_code, preservative_description, preservative_code_comment, preservative_description_short, preservative_note) %>%
  summarise(count = n()) -> summary_table

# Convert Trawl_number to integer instead of character
df_final_calc_end$trawl_number <- as.integer(df_final_calc_end$trawl_number)

### In the depth_m columns, correct values "98.109375", "14.83203125"
df_final_clean_depth <- df_final_calc_end %>%
  mutate(depth_m = case_when(depth_m == "98.109375" ~ NA_character_,
                             depth_m == "14.83203125" ~ NA_character_,
                              TRUE ~ depth_m))

# Flag any value greater than "100" and add the comment to a new column
# Correct rows with values "188" and "200" meters
df_final_clean_depth <- df_final_clean_depth %>%
  mutate(depth_m = as.numeric(depth_m)) %>%
  mutate(depth_m_flag = case_when(depth_m == 188 ~ "Likely typo, corrected from 188 m to 18 m",
                                  depth_m == 200 ~ "Likely typo, corrected from 200 m to 20 m",
                                  TRUE ~ NA_character_)) %>%
  mutate(depth_m = case_when(depth_m == 188 ~ 18,
                             depth_m == 200 ~ 20,
                             TRUE ~ depth_m)) %>%
  unite("depth_m_comments", depth_m_flag, depth_m_comment, sep = ", ", na.rm = TRUE)

### Clean scale, scale_book and scale_book_letter columns
# Replace wrong data in scale_book data for the correct number and flag modifications
df_final_clean_depth <- df_final_clean_depth %>%
  mutate(scale_book_comment = if_else(scale_book == "1.86" | scale_book == "0.49" | scale_book == "0.21",
                                      "Weight value was incorrectly recorded in the scale_book field; corrected by recoding to 0",
                                      NA_character_))

df_final_clean_depth <- df_final_clean_depth %>%
  mutate(scale_book = case_when(scale_book == "1.86" ~ "0",
                                scale_book == "0.49" ~ "0",
                                scale_book == "0.21" ~ "0",
                                 TRUE ~ scale_book))

# Transfer the scale_book comments to the scale_book_comment column
df_final_clean_depth <- df_final_clean_depth %>%
  mutate(
    ### extract the digits at the start
    scale_book_clean = str_extract(scale_book, "^\\w{1,3}"),
    
    ### extract the rest of the string as comment
    scale_book_comment = if_else(scale_book_comment == "" | is.na(scale_book_comment),
                                 str_trim(str_remove(scale_book, "^\\d{1,4}")),
                                          paste(scale_book_comment, str_trim(str_remove(scale_book, "^\\d{1,4}")), sep = "")),
    
    ### replace missing/empty comments with NA character
    scale_book_comment = if_else(scale_book_comment == "" | is.na(scale_book_comment) | scale_book_comment == " ",
                                 NA_character_, scale_book_comment),
    
    ### convert number to integer (optional, based on your needs)
    scale_book = scale_book_clean) %>%
  select(-scale_book_clean)

# Transfer scale book letters from scale_book column to scale_book_letter
df_final_clean_depth <- df_final_clean_depth %>%
  # Detect if there is a letter in the row and remove them
  mutate(
    scale_book_is_letter = str_detect(scale_book, "^[A-Za-z]$"),
    scale_book_flag_letter = if_else(scale_book_is_letter, scale_book, NA_character_),
    scale_book = if_else(!scale_book_is_letter, scale_book, NA_character_)) %>%
  # Compare if the letters are the same in the different columns: All rows match
  mutate(
    letters_match = case_when(is.na(scale_book_flag_letter) | is.na(scale_book_letter) ~ NA,
                              scale_book_flag_letter == scale_book_letter ~ TRUE,
                              TRUE ~ FALSE)) %>%
  # Replace missing data in the scale_book_letter by values from scale_book column
  mutate(
    scale_book_letter = if_else(is.na(scale_book_letter) & scale_book_is_letter, scale_book_flag_letter, scale_book_letter)) %>%
  # Delete extra rows
  select(-scale_book_flag_letter, -scale_book_flag_letter, -scale_book_is_letter, -letters_match)

### Comment values greater than 20 in the scale_book_comment column
df_final_clean_depth <- df_final_clean_depth  %>% 
  mutate(scale_book = as.integer(scale_book),
         scale_book_comment = if_else(scale_book > 20,
                                      if_else(is.na(scale_book_comment) | scale_book_comment == "",
                                              "scale_book > 20, possible error", 
                                              paste(scale_book_comment, "scale_book > 20, possible error", sep = "; ")),
                                      scale_book_comment))

df_final_clean_depth %>%
  group_by(scale, scale_book, scale_book_letter, scale_book_comment) %>%
  summarise(count = n()) -> summary_table

# Correct fish_unique_ID 1986-09-01_6_2_12_1_1_58_48 length and weight
nearly_final_dataframe <- df_final_clean_depth #%>%
 # mutate(fish_length_mm = case_when(fish_unique_ID == "1986-09-01_6_2_12_1_1_58_48" ~ 58,
 #                               TRUE ~ fish_length_mm),
 #        fish_weight_g = case_when(fish_unique_ID == "1986-09-01_6_2_12_1_1_58_48" ~ 1.86,
 #                                  TRUE ~ fish_weight_g))

# Detect seine comments in the trawl_comment column
sein_info_trawl_location <- grepl("sein", nearly_final_dataframe$trawl_comment, ignore.case = TRUE)
sum(sein_info_trawl_location == "TRUE", na.rm = TRUE)

# Create a new column to store seine info nested in trawl_comment column

# Include the default gear type as "Trawl" in the gear_type column
nearly_final_dataframe <- nearly_final_dataframe %>%
  mutate(gear_type = case_when(sein_info_trawl_location =="TRUE" ~ "Beach seine", 
                               #sample_type == "DUM2" | sample_type == "dum2" ~ NA_character_,
                               #sample_type == "7*7*7.5" ~ NA_character_,
                               TRUE ~ "Trawl"),
         sample_number = case_when(sample_number == "DUM1" | sample_number == "dum1" ~ NA_character_,
                                   TRUE ~ sample_number),
         gear_type_comment = case_when(sample_type == "DUM2" | sample_type == "dum2" ~ "Likely a test trawl",
                                       sample_type == "7*7*7.5" ~ "Likely wrong net dimensions",
                                       gear_type == "Beach seine" ~ "Gear type description provided in trawl_comment column",
                                       TRUE ~ NA_character_))

# Clean trawl_comment column
nearly_final_dataframe <- nearly_final_dataframe %>%
  mutate(trawl_comment = str_trim(str_remove(trawl_comment, "XXXXXXXXXXXXXXXXXXXXXXXXXXXX")),
         trawl_comment = gsub("\\s+", " ", trawl_comment),
         time_comment = ifelse(time_comment == "NANA", NA_character_, time_comment))

nearly_final_dataframe %>%
  group_by(sample_type, gear_type, sample_number) %>%
  summarise(count = n()) -> summary_table

################### Cleaning Fish length and weight columns #########################

# Detect problematic values in weight and length columns
Fish_id_filters <- c("1987-11-26_107_4_16_2_23_1.74_555", "1987-07-31_29_5_0_2_2_1.05_0.49", "1987-09-11_21_1_9_2_46_25.6_67",
                     "1992-08-17_214_2_4_2_156_0.23_0.33", "1993-02-25_3_3_55_2_1_96_45", "1998-09-24_801_11_17_7_46_35_32",
                     "1985-12-11_41_4_15_2_16_0.34_3", "1987-02-25_41_1_7_27_2_0.19_0.3", "1984-09-01_25_2_10_1_2_5.29_28",
                     "1997-02-20_41_5_8_26_72_0.11_0.28", "1989-06-03_41_15_10_7_78_30_0.26", "1984-09-10_69_4_5_2_53_1.45_25",
                     "1987-09-09_8_8_20_1_135_0.66_400", "1993-07-24_8_4_16_7_46_60_39", "1989-05-14_62_9_7_5_21_0.7_20",
                     "1987-09-09_8_8_20_1_146_0.64_399", "1987-09-09_8_8_20_1_21_1.31_500", "1991-09-11_64_18_15_7_44_17.3_58",
                     "1987-09-09_8_9_7_2_81_0.84_444", "1987-09-09_8_10_7_2_55_1.46_544", "1999-09-08_802_2_8_7_134_58_39",
                     "1987-09-09_8_13_20_1_42_1.033_455", "1987-09-09_8_13_20_1_54_1.36_500", "1977-08-16_40_1_10_2_10_0.09_5",
                     "1995-08-30_8_7_7_2_57_999_4", "1990-06-22_802_1_6_2_48_0.46_0.38", "1984-06-08_6_2_10_1_44_0.34_24",
                     "1999-09-08_802_2_8_7_268_0_0", "1987-11-29_118_8_20_9_1_0_1.88", "1980-08-14_8_3_17_1_11_0.55_27",
                     "1989-02-22_44_3_30_1_3_0.73_4", "1995-08-26_18_19_11_7_21_0.61_0.37", "1981-02-03_41_2_40_2_97_1.4_28",
                     "1995-08-26_18_11_0_7_137_46_35", "1995-08-26_18_13_7_7_93_39_53", "1981-07-19_31_4_5_2_73_0.33_95",
                     "1991-03-12_18_2_7_2_8_36_33", "1996-08-22_18_25_0_2_70_0.11_2.5", "1981-08-16_21_1_30_1_26_0.81_33",
                     "1996-08-22_18_25_0_2_71_0.16_2.7", "1991-09-14_66_99_10_7_2_1.13_5", "1981-09-04_41_4_12_2_272_1.17_31",
                     "1992-08-12_23_4_31_7_36_90_45", "1992-08-12_22_1_24_7_133_97_46", "1981-09-20_31_2_10_2_144_1.41_37",
                     "1992-08-12_22_1_24_7_126_66_41", "1992-08-12_22_2_28_7_134_66_42", "1981-09-21_28_1_0_2_78_1.87_45",
                     "1992-08-12_22_2_28_7_133_105_46", "1984-10-15_1_1_0_1_1_1.13_0", "1981-09-21_28_1_0_2_80_1.48_38",
                     "1989-06-29_1_1_10_7_50_0.16_0.27", "1986-09-01_6_2_12_1_1_58_48", "1981-12-15_41_2_30_2_590_0.75_30",
                     "1993-07-25_8_8_12_7_34_0.42_0.35", "1984-06-01_6_2_7_1_31_0.21_0.29", "1981-12-15_41_2_30_2_727_1.61_41",
                     "1993-07-25_8_8_12_7_112_0.23_0.3", "1986-09-01_6_2_12_1_43_0.82_0.8", "1982-07-21_3_4_22_1_1_1.13_27",
                     "1977-08-16_40_2_17_2_8_0.14_1", "1977-08-16_40_1_10_2_12_1.53_3", "1982-08-13_6_2_20_1_10_1.75_42",
                     "1977-08-16_40_2_17_2_9_0.14_2", "1992-07-15_8_16_13_7_6_0.88_4", "1982-08-13_6_3_13_1_11_1.45_35",
                     "1992-10-12_8_4_19_2_6_1.92_6", "1977-08-16_40_1_10_2_13_1.53_6", "1983-08-29_27_3_12_2_3_5.51_61",
                     "1977-08-16_40_2_17_2_10_0.13_4", "1977-08-16_40_1_10_2_11_1.19_9", "1983-08-30_27_5_30_2_1_5.77_62",
                     "1990-07-23_107_9_10_7_28_75_40", "1979-07-26_41_1_12_2_12_0.02_44", "1983-09-15_40_4_9_2_255_0.36_24",
                     "1981-07-19_31_3_0_2_134_0.09_71", "1981-07-19_31_4_5_2_73_0.33_95", "1983-10-04_41_2_25_1_83_1.64_40",
                     "1981-09-02_40_3_5_2_12_0.11_60", "1979-07-27_41_2_8_2_250_0.06_47", "1984-04-25_6_4_13_1_12_0.19_17",
                     "1987-08-10_21_1_12_9_1_5_154", "1987-07-30_29_3_10_1_35_56_39", "1984-05-02_6_4_10_1_16_0.23_21",
                     "1987-07-30_29_3_10_1_8_52_38", "1986-08-31_6_1_12_1_5_71_44", "1993-07-26_8_10_3_2_5_3.27_51",
                     "1984-06-18_6_3_11_1_18_45_35", "1984-06-18_6_3_11_1_19_29_31", "1995-06-21_3_3_18_7_4_1.8_30",
                     "1984-06-27_40_15_6_2_2_58_39", "1993-02-25_3_3_55_2_3_75_41", "1995-08-25_18_6_0_7_93_0.89_32",
                     "1989-06-25_8_1_0_2_66_37_35", "1990-03-01_8_1_9_2_154_34_35", "1995-08-26_18_13_7_7_93_39_53",
                     "1990-09-13_801_9_10_2_8_33_34", "1987-07-18_8_4_7_2_7_8.3_46", "1995-08-27_18_20_0_2_200_0.19_17",
                     "1977-08-16_40_1_10_2_11_1.19_9", "1984-06-14_40_20_5_1_1_0.45_26", "1995-08-27_18_20_0_2_32_0.04_11",
                     "1977-08-16_40_1_10_2_12_1.53_3", "1984-06-28_44_11_7_1_6_0.38_24", "1995-08-27_18_21_0_2_54_2.61_48",
                     "1977-08-16_40_1_10_2_13_1.53_6", "1984-07-03_40_4_7_1_2_1.44_41", "1995-08-27_18_24_7_7_41_0.96_36",
                     "1977-08-16_40_2_17_2_1_0.1_6", "1984-07-25_40_6_10_1_51_0.91_32", "1995-08-28_18_37_0_7_29_3_34",
                     "1977-08-16_40_2_17_2_10_0.13_4", "1984-07-25_40_7_10_1_138_1.66_42", "1995-08-30_8_6_21_7_40_1.39_39",
                     "1977-08-16_40_2_17_2_2_0.15_8", "1984-08-27_31_1_5_2_49_1.94_45", "1995-10-13_3_17_22_2_1_0.61_31",
                     "1977-08-16_40_2_17_2_3_0.12_8", "1984-08-31_29_6_12_1_6_1.78_43", "1996-07-31_6_3_14_7_4_2.67_42",
                     "1977-08-16_40_2_17_2_4_0.12_9", "1984-09-01_25_2_10_1_2_5.29_28", "1996-08-22_18_25_0_2_22_2.3_47",
                     "1977-08-16_40_2_17_2_5_0.12_9", "1984-09-06_32_6_7_1_34_0.61_31", "1997-09-23_62_12_15_7_2_2.82_52",
                     "1977-08-16_40_2_17_2_8_0.14_1", "1984-09-14_8_6_10_1_3_0.61_31", "1997-11-20_1_7_17_7_1_2.4_47",
                     "1977-08-17_40_3_13_2_1_3.14_33", "1984-09-15_8_19_5_2_164_1.95_46", "1984-05-03_40_15_4_1_53_0.23_21",
                     "1977-09-21_40_4_15_2_65_3.34_52", "1985-09-06_62_2_12_2_41_2.78_32", "1984-05-11_6_3_8_1_26_0.2_21",
                     "1978-08-16_40_3_16_2_1_0.53_18", "1985-09-09_29_11_5_2_33_1.61_24", "1984-06-08_6_2_10_1_35_0.5_27",
                     "1978-08-16_40_3_16_2_2_0.61_20", "1985-09-09_29_4_5_2_166_1.46_26", "1989-06-22_3_2_17_7_55_1_26",
                     "1978-08-16_40_4_13_1_117_2.17_30", "1985-09-09_29_4_5_2_328_0.87_22", "1989-07-02_107_9_9_7_2_0.54_28",
                     "1978-10-05_11_2_25_1_2_1.92_45", "1985-09-09_29_4_5_2_73_2.22_30", "1991-10-24_41_3_12_2_33_4.2_36",
                     "1978-11-15_40_3_42_2_7_0.28_12", "1986-07-26_23_7_16_1_2_1.6_41", "1992-07-14_8_4_16_7_42_0.66_32",
                     "1978-11-30_40_5_38_2_12_0.76_30", "1986-08-13_69_4_11_2_3_3.11_50", "1993-07-17_3_2_11_7_21_0.97_33",
                     "1978-11-30_40_5_38_2_13_0.6_31", "1986-08-13_69_9_11_1_1_1.17_37", "1979-09-12_41_2_6_2_36_0.19_7",
                     "1979-07-26_41_1_12_2_12_0.02_44", "1986-09-01_6_2_12_1_43_0.82_0.8", "1979-09-12_41_2_6_2_37_0.19_7",
                     "1979-07-27_41_2_8_2_207_1.96_46", "1987-07-30_29_3_10_2_1_5.5_39",  "1979-10-04_31_1_14_2_104_0.78_29",
                     "1979-09-12_41_2_6_2_30_0.19_7", "1987-07-30_29_3_10_2_2_8.8_46",  "1979-10-04_31_1_14_2_105_0.76_27",
                     "1979-09-12_41_2_6_2_31_0.2_7", "1987-07-30_29_3_10_2_3_6.2_40",  "1979-11-06_40_2_25_2_13_1.2_26",
                     "1979-09-12_41_2_6_2_32_0.18_7", "1988-07-20_40_3_0_9_1_2.43_43", "1989-06-01_41_3_0_2_2_0.42_25",
                     "1979-09-12_41_2_6_2_33_0.14_7", "1988-09-15_8_6_20_1_31_2_43", "1979-09-12_41_2_6_2_35_0.19_7",
                     "1979-09-12_41_2_6_2_34_0.19_7", "1988-09-20_24_7_19_9_1_6.16_62")

# Filter the data frame
filtered_df <- filter(nearly_final_dataframe, fish_unique_ID %in% Fish_id_filters)

# Save document until here
write.csv(filtered_df, paste0(error_directory, "/fish_length_weight_errors.csv"), row.names = FALSE)

# Clean inconsistent length and weight data
# Read lookup table with the corrected values and specific comments
length_weight_lookup_table <- read.csv("./TRAWL_BIOSAMPLE/00_raw_data/03_AA_look_up_tables/length_weight_error_corrections.csv")

# Use the lookup table to correct the values in the final dataset
df_final_calc_std_weight <- nearly_final_dataframe %>%
  dplyr::left_join(length_weight_lookup_table, by = "fish_unique_ID") %>%
  # Flag changes in the comment column
  mutate(length_weight_comment = case_when(is.na(length_weight_comment) & fish_weight_g == 0 ~ "Weight = 0 replaced by NA",
                                           is.na(length_weight_comment) & fish_weight_g > 100.00 ~ "Weight > 100g, likely error, replaced by NA",
                                           is.na(length_weight_comment) & fish_length_mm == 0 ~ "length = 0 replaced by NA",
                                           is.na(length_weight_comment) & fish_length_mm > 300 ~ "length > 300 mm, likely error, replaced by NA",
                                           TRUE ~ length_weight_comment),
          # Replacement commands for fish length and weight columns
         fish_length_mm = case_when(!is.na(old_value_length) ~ new_value_length,
                                    fish_length_mm == 0 ~ NA_real_,
                                    fish_length_mm > 300 ~ NA_real_,
                                    TRUE ~ fish_length_mm),
         fish_weight_g = case_when(!is.na(old_value_weight) ~ new_value_weight,
                              fish_weight_g == 0 ~ NA_real_,
                              fish_weight_g > 100.00 ~ NA_real_,
                              TRUE ~ fish_weight_g)) %>%
  select(-old_value_weight, -old_value_length, -new_value_length, -new_value_weight)
      

df_final_calc_std_weight %>%
  group_by(fish_length_mm, fish_weight_g, length_weight_comment) %>%
  summarise(count = n()) -> summary_table_length

# Filter the data frame
filtered_df_fixed <- filter(df_final_calc_std_weight, fish_unique_ID %in% Fish_id_filters)

# Calculate standardized weight using a lookup table
#standardized_weight_lookup_table <- read.csv("./TRAWL_BIOSAMPLE/00_raw_data/03_AA_look_up_tables/AA_calc_std_weight.csv")
df_final_calc_std_weight <- df_final_calc_std_weight %>%
  #dplyr::left_join(standardized_weight_lookup_table, by = c("preservative_code", "weight_conversion_formula")) %>%
  mutate(calc_std_weight_g = fish_weight_g / calc_value)

# Round the new column, calc_std_weight_g, to two decimal places
df_final_calc_std_weight$calc_std_weight_g <- round(df_final_calc_std_weight$calc_std_weight_g, digits = 2)

# Compare them to the existing standardized_weight_g column
final_dataframe <- df_final_calc_std_weight %>%
  mutate(std_weight_g_comment = case_when(
    # standardized_weight_g exists and matches calc_std_weight_g
    !is.na(standardized_weight_g) & !is.na(calc_std_weight_g) &
      abs(standardized_weight_g - calc_std_weight_g) < 1 ~ "matches calc_std_weight_g",
    # standardized_weight_g exists but does NOT match calc_std_weight_g
    !is.na(standardized_weight_g) & !is.na(calc_std_weight_g) &
      abs(standardized_weight_g - calc_std_weight_g) >= 1 ~ "does NOT match calc_std_weight_g",
    # standardized_weight_g calculated from calc_std_weight_g
    is.na(standardized_weight_g) & !is.na(calc_std_weight_g) ~ "standardized_weight_g calculated from standardized weight formula",
    # nothing possible
    TRUE ~ "standardized weight could not be calculated"))

# Check number of problematic rows
sum(final_dataframe$std_weight_g_comment == "does NOT match calc_std_weight_g", na.rm = TRUE)
sum(final_dataframe$std_weight_g_comment == "standardized_weight_g calculated from standardized weight formula", na.rm = TRUE)
sum(final_dataframe$std_weight_g_comment == "standardized weight could not be calculated", na.rm = TRUE)

# Export errors in standardized weight
std_weight_errors <- final_dataframe[!is.na(final_dataframe$std_weight_g_comment) 
                                     & final_dataframe$std_weight_g_comment == "does NOT match calculated standardized weight",]
write.csv(std_weight_errors, paste0(error_directory, "/std_weight_errors.csv"), row.names = FALSE)

# Calculate K factor and include it as a new column
final_dataframe <- final_dataframe %>%
  mutate(length_cm = fish_length_mm / 10, 
         calc_K_factor = 100 * calc_std_weight_g / (length_cm^3)) %>%
  select(-length_cm)

# Clear start_time, depth, duration and trawl_number
final_dataframe <- final_dataframe %>%
  mutate(trawl_number = case_when(sample_type == "DUM2" | sample_type == "dum2" ~ NA_real_,
                                   TRUE ~ trawl_number),
         start_time = case_when(sample_type == "DUM2" | sample_type == "dum2" ~ NA_character_,
                                       TRUE ~ start_time),
         depth_m = case_when(sample_type == "DUM2" | sample_type == "dum2" ~ NA_real_,
                             TRUE ~ depth_m),
         duration_mi = case_when(sample_type == "DUM2" | sample_type == "dum2" ~ NA_real_,
                             TRUE ~ duration_mi),
         test_trawl_comment = case_when(sample_type == "DUM2" | sample_type == "dum2" ~ 
                                          "Fake start_time, depth_m, duration_minutes and trawl_number. Set to NA.",
                                        TRUE ~ NA_character_))

# Correct start_time for specific trawls
final_dataframe <- final_dataframe %>%
  mutate(start_time = case_when(trawl_unique_ID == "1988-07-31_29_6_0" ~ "03:00:00",
                                trawl_unique_ID == "1992-08-12_22_2_28" ~ "22:30:00",
                                trawl_unique_ID == "1992-08-12_22_3_32" ~ "22:45:00",
                                trawl_unique_ID == "1992-08-12_22_4_0" ~ "23:00:00",
                                TRUE ~ start_time),
         start_time = round_hms(as_hms(start_time), 60),
         start_time_comment = case_when(trawl_unique_ID == "1988-07-31_29_6_0" ~ "Start_time inferred from interval between previous and next trawl",
                                        trawl_unique_ID == "1992-08-12_22_2_28" ~ "Start_time inferred from interval between previous and next trawl",
                                        trawl_unique_ID == "1992-08-12_22_3_32" ~ "Start_time inferred from interval between previous and next trawl",
                                        trawl_unique_ID == "1992-08-12_22_4_0" ~ "Start_time inferred from interval between previous and next trawl",
                                        TRUE ~ NA_character_))

# Save document until here
write.csv(final_dataframe, paste0(working_directory, "/combined_inprogress_df_trawl.csv"), row.names = FALSE)

# Combine columns flagging issue in a data_issue column and general comments in a general_comments column, 
# skip the row when it is NA
final_dataframe <- final_dataframe %>%
  rowwise() %>%
  mutate(data_issues = paste(
      c(if (duration_comment != "matches calculated start_time and end_time" & 
            duration_comment != "duration not provided, calculated from start_time, end_time" &
            duration_comment != "duration could not be calculated") paste0("duration_comment: ", duration_comment),
        if (!is.na(length_weight_comment) & length_weight_comment != "") paste0("length_weight_comment: ", length_weight_comment),
        if (!is.na(duplicate_flag) & duplicate_flag != "") paste0("duplicate_flag: ", duplicate_flag),
        if (!is.na(no_species_name_comments) & no_species_name_comments != "") paste0("no_species_name_comments: ", no_species_name_comments),
        if (!is.na(gear_type_comment) & gear_type_comment != "") paste0("gear_type_comment: ", gear_type_comment),
        if (!is.na(scale_book_comment) & scale_book_comment != "") paste0("scale_book_comment: ", scale_book_comment),
        if (!is.na(std_weight_g_comment) & std_weight_g_comment == "does NOT match calc_std_weight_g") paste0("std_weight_g_comment: ", std_weight_g_comment),
        if (!is.na(calc_end_time_comment) & calc_end_time_comment == "end_time missing, calculated from start_time + duration_mi") paste0("calc_end_time_comment: ", calc_end_time_comment),
        if (!is.na(test_trawl_comment) & test_trawl_comment != "") paste0("test_trawl_comment: ", test_trawl_comment),
        if (!is.na(start_time_comment) & start_time_comment != "") paste0("start_time_comment: ", start_time_comment),
        if (!is.na(invalid_duration_time) & invalid_duration_time != "") paste0("invalid_duration_time: ", invalid_duration_time),
        if (!is.na(invalid_start_time) & invalid_start_time == "Invalid format") paste0("invalid_start_time: ", invalid_start_time),
        if (!is.na(invalid_end_time) & invalid_end_time != "") paste0("invalid_end_time: ", invalid_end_time)),
      collapse = "; "
    )) %>%
  mutate(general_comments = paste(
    c(if (!is.na(trawl_date_comment) & trawl_date_comment != "") paste0("trawl_date_comment: ", trawl_date_comment),
      if (!is.na(trawl_number_comment) & trawl_number_comment != "") paste0("trawl_number_comment: ", trawl_number_comment),
      if (!is.na(trawl_comment) & trawl_comment != "") paste0("trawl_comment: ", trawl_comment),
      if (!is.na(comment) & comment != "") paste0("comment: ", comment),
      if (!is.na(time_comment) & time_comment != "") paste0("time_comment: ", time_comment),
      if (!is.na(preservative_note) & preservative_note != "") paste0("preservative_note: ", preservative_note),
      if (!is.na(depth_m_comments) & depth_m_comments != "") paste0("depth_m_comments: ", depth_m_comments),
      if (!is.na(preservative_code_comment) & preservative_code_comment != "") paste0("preservative_code_comment: ", preservative_code_comment)),
    collapse = "; "
  )) %>%
  mutate(data_validation_comments = paste(
    c(if (!is.na(merging_update_type) & merging_update_type != "") paste0("merging_update_type: ", merging_update_type),
      if (duration_comment == "duration could not be calculated") paste0("duration_comment: ", duration_comment),
      if (std_weight_g_comment != "does NOT match calc_std_weight_g") paste0("std_weight_g_comment: ", std_weight_g_comment)),
    collapse = "; "
  )) %>%
  ungroup()  %>%
  select(-trawl_comment, -comment, -time_comment, -preservative_code_comment, -depth_m_comments, -invalid_end_time,
        -trawl_date_comment, -trawl_number_comment, -merging_update_type, -scale_book_comment, -calc_value,
        -duration_comment, -invalid_duration_time, -invalid_start_time, -calc_end_time_comment, -duration_final, 
        -std_weight_g_comment, -gear_type_comment, -length_weight_comment, -no_species_name_comments, -preservative_note, 
        -duplicate_flag, -test_trawl_comment, -start_time_comment)

# Get the time stamp to record when the data was rescued
time_stamp <- format(Sys.time(), "%d-%b-%Y %H:%M") # get time-stamp to indicate when data rescued

# Reorganize and rename columns
final_dataframe <- final_dataframe %>%
  mutate(time_stamp = time_stamp) %>%
  unite("program_notes", program_notes, time_stamp, sep = ". ", na.rm = TRUE) %>%
  dplyr::select(ats_year, lake_code, lake_name, trawl_date, trawl_month, 
              trawl_number, sample_type, gear_type,
              depth_m, "orig_start_time" = start_time, "orig_end_time" = end_time, calc_end_time, 
              "orig_duration_minutes" = duration_mi, calc_duration_time, 
              fish_id, species_code, "species_common_name" = fish_description,  
              "life_stage" = species_code_comment, species_info_code, 
              fish_length_mm, fish_weight_g, "orig_std_weight_g" = standardized_weight_g, calc_std_weight_g, calc_K_factor,
              preservative_code, preservative_description, preservative_description_short, 
              weight_conversion_formula, "age_class" = age, aging_technique, aging_technique_name,
              scale, scale_book, scale_book_letter,
              general_comments, data_issues,
              source_files, source_line,
              trawl_unique_ID, fish_unique_ID,
              genus_name, species_name, 
              lake_latitude, lake_longitude,
              processor, process_date,
              everything(),                    
              program_notes,     # timestamp when data was rescued
              -.joyn) %>%
  arrange(ats_year, lake_name, trawl_date, trawl_number, fish_id)

# Save document until here
write.csv(final_dataframe, paste0(final_directory, "/Trawl_data_FINAL_1977-1999.csv"), row.names = FALSE)
