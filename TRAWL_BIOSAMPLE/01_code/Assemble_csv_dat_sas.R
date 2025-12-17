###############################################################################
##############                  TRAWL data                  ###################
##############          assembling .csv trawl files         ###################
############## Authors: Alice Assmar (McGill Uni.), David   ###################
############## Hunt (McGill Uni.),  Yuliya Shtymburski      ###################
############## (U. Regina), Howard Stiff (DFO Nanaimo),     ###################
##############        Athena Ogden (DFO Nanaimo)            ###################
###############################################################################

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
# Testing the values using Trawl 84'
#### Counting Occurrences of All Unique Values in a Column:
#table(Trawl_84_sas$fish_unique_ID)
#Trawl_84_sas[Trawl_84_sas$fish_unique_ID == "1984-06-26_40_6_6_2_1_2.52_63", ]
#Trawl_84_dat[Trawl_84_dat$fish_unique_ID == "1984-06-26_40_6_6_2_1_2.52_63", ]
#Trawl_84[Trawl_84$fish_unique_ID == "1984-06-26_40_6_6_2_1_2.52_63", ]
#
#Trawl_84_sas[Trawl_84_sas$fish_unique_ID == "1984-05-01_40_1_2_2_6_0.22_32", ]
#Trawl_84_dat[Trawl_84_dat$fish_unique_ID == "1984-05-01_40_1_2_2_6_0.22_32", ]
#Trawl_84[Trawl_84$fish_unique_ID == "1984-05-01_40_1_2_2_6_0.22_32", ]
#
#Trawl_84_sas[Trawl_84_sas$fish_unique_ID == "1984-04-25_6_1_4_1_1_3.12_67", ]
#Trawl_84_dat[Trawl_84_dat$fish_unique_ID == "1984-04-25_6_1_4_1_1_3.12_67", ]
#Trawl_84[Trawl_84$fish_unique_ID == "1984-04-25_6_1_4_1_1_3.12_67", ]
#
# If I do not use all column names, it will keep duplicates of all columns
#Trawl_84_rows <- joyn::full_join(Trawl_84_sas, Trawl_84_dat,  suffix = c(".sas", ".dat"),
#                      by = c("fish_unique_ID", "fish_total"), update_values = TRUE)

#Trawl_84_rows <- joyn::full_join(Trawl_84_sas, Trawl_84_dat,  suffix = c(".sas", ".dat"),
#                                 by = c("fish_unique_ID", "fish_total"))


# combining several columns
Trawl_84 <- joyn::full_join(Trawl_84_sas, Trawl_84_dat, suffix = c(".sas", ".dat"),
                            by = c("process_date", "trawl_date", "fish_total",
                                   "fish_length_mm", "trawl_unique_ID", "fish_unique_ID", "species_code",
                                   "trawl_number", "processor", "lake_code", "lake_name",
                                   "scale_book", "scale", "age",
                                   "preservative_code", "duration_mi", "depth_m", "fish_id",
                                   "fish_description", "preservative_description", "weight_conversion_formula",
                                   "sample_number", "fish_weight_g", "aging_technique_name",
                                   "aging_technique", "trawl_month", "ats_year", "program_notes", "species_code_comment"), update_values = TRUE)

# Combining Trawl of the following years
Trawl_88 <- joyn::full_join(Trawl_88_dat, Trawl_88_sas,suffix = c(".sas", ".dat"),
                            by = c("process_date", "trawl_date", "fish_total",
                                   "fish_length_mm", "trawl_unique_ID", "fish_unique_ID", "species_code",
                                   "trawl_number", "processor", "lake_code", "lake_name",
                                   "scale_book", "scale", "age",
                                   "preservative_code", "duration_mi", "depth_m", "fish_id",
                                   "fish_description", "preservative_description", "weight_conversion_formula",
                                   "sample_number", "fish_weight_g", "aging_technique_name",
                                   "aging_technique", "trawl_month", "ats_year", "program_notes", "species_code_comment"), update_values = TRUE)

Trawl_89 <- joyn::full_join(Trawl_89_sas, Trawl_89_dat,  suffix = c(".sas", ".dat"),
                      by = c("process_date", "trawl_date", "fish_total",
                             "fish_length_mm", "trawl_unique_ID", "fish_unique_ID", "species_code",
                             "trawl_number", "processor", "lake_code", "lake_name",
                             "scale_book", "scale", "age",
                             "preservative_code", "duration_mi", "depth_m", "fish_id",
                             "fish_description", "preservative_description", "weight_conversion_formula",
                             "sample_number", "fish_weight_g", "aging_technique_name",
                             "aging_technique", "trawl_month", "ats_year", "program_notes", "species_code_comment"), update_values = TRUE)

Trawl_90 <- joyn::full_join(Trawl_90_sas, Trawl_90_dat,  suffix = c(".sas", ".dat"),
                      by = c("process_date", "trawl_date", "fish_total",
                             "fish_length_mm", "trawl_unique_ID", "fish_unique_ID", "species_code",
                             "trawl_number", "processor", "lake_code", "lake_name",
                             "scale_book", "scale", "age",
                             "preservative_code", "duration_mi", "depth_m", "fish_id",
                             "fish_description", "preservative_description", "weight_conversion_formula",
                             "sample_number", "fish_weight_g", "aging_technique_name",
                             "aging_technique", "trawl_month", "ats_year", "program_notes", "species_code_comment"), update_values = TRUE)

Trawl_91 <- joyn::full_join(Trawl_91_sas, Trawl_91_dat,  suffix = c(".sas", ".dat"),
                      by = c("process_date", "trawl_date", "fish_total",
                             "fish_length_mm", "trawl_unique_ID", "fish_unique_ID", "species_code",
                             "trawl_number", "processor", "lake_code", "lake_name",
                             "scale_book", "scale", "age",
                             "preservative_code", "duration_mi", "depth_m", "fish_id",
                             "fish_description", "preservative_description", "weight_conversion_formula",
                             "sample_number", "fish_weight_g", "aging_technique_name",
                             "aging_technique", "trawl_month", "ats_year", "program_notes", "species_code_comment"), update_values = TRUE)

Trawl_92 <- joyn::full_join(Trawl_92_sas, Trawl_92_dat, suffix = c(".sas", ".dat"),
                      by = c("process_date", "trawl_date", "fish_total",
                             "fish_length_mm", "trawl_unique_ID", "fish_unique_ID", "species_code",
                             "trawl_number", "processor", "lake_code", "lake_name",
                             "scale_book", "scale", "age",
                             "preservative_code", "duration_mi", "depth_m", "fish_id",
                             "fish_description", "preservative_description", "weight_conversion_formula",
                             "sample_number", "fish_weight_g", "aging_technique_name",
                             "aging_technique", "trawl_month", "ats_year", "program_notes", "species_code_comment"), update_values = TRUE)

Trawl_93 <- joyn::full_join(Trawl_93_sas, Trawl_93_dat,  suffix = c(".sas", ".dat"),
                      by = c("process_date", "trawl_date", "fish_total",
                             "fish_length_mm", "trawl_unique_ID", "fish_unique_ID", "species_code",
                             "trawl_number", "processor", "lake_code", "lake_name",
                             "scale_book", "scale", "age",
                             "preservative_code", "duration_mi", "depth_m", "fish_id",
                             "fish_description", "preservative_description", "weight_conversion_formula",
                             "sample_number", "fish_weight_g", "aging_technique_name",
                             "aging_technique", "trawl_month", "ats_year", "program_notes", "species_code_comment"), update_values = TRUE)

# Remove duplicates
Trawl_95_dat <- Trawl_95_dat[!duplicated(Trawl_95_dat$fish_unique_ID), ]
#Trawl_95_rows <- joyn::full_join(Trawl_95_sas, Trawl_95_dat,  suffix = c(".sas", ".dat"),
#                                 by = "fish_unique_ID", update_values = TRUE)

Trawl_95 <- joyn::full_join(Trawl_95_sas, Trawl_95_dat,  suffix = c(".sas", ".dat"),
                      by = c("process_date", "trawl_date", "fish_total",
                             "fish_length_mm", "trawl_unique_ID", "fish_unique_ID", "species_code",
                             "trawl_number", "processor", "lake_code", "lake_name",
                             "start_time", "end_time", "scale_book", "scale", "age",
                             "preservative_code", "duration_mi", "depth_m", "fish_id",
                             "fish_description", "preservative_description", "weight_conversion_formula",
                             "sample_number", "fish_weight_g", "aging_technique_name",
                             "aging_technique", "trawl_month", "ats_year", "program_notes", "species_code_comment"), update_values = TRUE)

Trawl_96 <- joyn::full_join(Trawl_96_sas, Trawl_96_dat,  suffix = c(".sas", ".dat"),
                      by = c("process_date", "trawl_date", "fish_total",
                             "fish_length_mm", "trawl_unique_ID", "fish_unique_ID", "species_code",
                             "trawl_number", "processor", "lake_code", "lake_name",
                             "scale_book", "scale", "age",
                             "preservative_code", "duration_mi", "depth_m", "fish_id",
                             "fish_description", "preservative_description", "weight_conversion_formula",
                             "sample_number", "fish_weight_g", "aging_technique_name",
                             "aging_technique", "trawl_month", "ats_year", "program_notes", "species_code_comment"), update_values = TRUE)


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

# Apply the function to all data frames in the list for duration_m
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

# Check duplicated values in fish unique ID
table(df_joined$fish_unique_ID)
df_joined_dup <- df_joined[duplicated(df_joined$fish_unique_ID), ]

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

df_joined <- as.data.frame(df_joined)

### Flag invalid rows for start_time
# Define the regex pattern for HH:MM:SS format
time_pattern <- "^([01]?[0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]$"

# Use grepl() to create a logical vector indicating valid formats
is_valid <- ifelse(is.na(df_joined$start_time.dat), NA, grepl(time_pattern, df_joined$start_time.dat))

# Add a new column to the data frame to flag invalid entries
df_joined$dat_invalid_start_time <- if_else(is_valid, "Valid", "Invalid")

# Check problematic rows
sum(df_joined$dat_invalid_start_time == "Invalid", na.rm = TRUE)

# Correct the format of the .dat column time, and merge all start_time and end_time columns




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
df_final <- df_joined %>%
  group_by(fish_unique_ID, fish_total) %>%
  summarise(
    # Keep source column explicitly
    source_file.dat = first(source_file.dat),
    # Compute flags and add to a column called "merging_update_type"
    merging_update_type = {
      # Single-row group
      if (n() == 1) {
        "Single-row"
      } else {
        flags <- map_chr(
          #cur_data() %>% select(-source_file.dat), # this deletes the source_file.dat column 
          pick(-source_file.dat),
          ~ resolve_column(.x, source_file.dat)$status
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
      -source_file.dat,
      ~ resolve_column(.x, source_file.dat)$value
    ),
  
    .groups = "drop"
  )


### Example of conflicting rows to test
# Before concatenation of rows
df_joined[df_joined$fish_unique_ID == "1984-06-26_40_6_6_2_1_2.52_63", ] # seems duplicates, but it isn't
df_joined[df_joined$fish_unique_ID == "1984-05-01_40_1_2_2_6_0.22_32", ] # duplicates
df_joined[df_joined$fish_unique_ID == "1984-04-25_6_1_4_1_1_3.12_67", ] # duplicates
df_joined[df_joined$fish_unique_ID == "1989-06-01_41_3_0_2_1_1.72_59", ] # 
df_joined[df_joined$fish_unique_ID == "1984-10-02_2_4_15_1_3_2.6_65", ] # start time problems
df_joined[df_joined$fish_unique_ID == "1989-07-29_107_4_9_7_15_0.38_33", ] # start time problems
df_joined[df_joined$fish_unique_ID == "1989-05-14_62_7_6_7_2_0.21_28", ] # start time problems

# After concatenation
unique(df_final$merging_update_type)
table(df_final$merging_update_type)
df_final <- as.data.frame(df_final)
df_final[df_final$fish_unique_ID == "1984-06-26_40_6_6_2_1_2.52_63", ]
df_final[df_final$fish_unique_ID == "1984-05-01_40_1_2_2_6_0.22_32", ]
df_final[df_final$fish_unique_ID == "1984-04-25_6_1_4_1_1_3.12_67", ]






### Combine source_file source_file.sas and source_file.dat into a new 'source_files' column
df_final <- df_final %>%
  mutate(source_files = coalesce(source_file, source_file.sas, source_file.dat)) %>%
  select(-source_file, -source_file.sas, -source_file.dat)

unique(df_final$source_files)

### Combine start_time.sas and .dat into a new 'start_time' column - .sas files have the time set better formated
## Before combine columns, flag the ones with errors and correct them

## Combine
df_final <- df_joined %>%
  mutate(source_files = coalesce(source_file, source_file.sas, source_file.dat)) %>%
  select(-source_file, -source_file.sas, -source_file.dat)

unique(df_final$source_files)


### Drop unnecessary columns and reorganize column order.

# Save combined data frame
write.csv(df_final, paste0(working_directory, "/combined_df_trawl.csv"), row.names = FALSE)


