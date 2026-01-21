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
error_directory <- "./TRAWL_BIOSAMPLE/03_errors_out/"

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
table(df_joined$fish_unique_ID)
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
df_final <- df_joined %>%
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

df_final_backup <- df_final

### Combine source_file, source_file.sas and source_file.dat into a new 'source_files' column
df_final <- df_final %>%
  unite(col = "source_files", source_file, source_file.sas, source_file.dat, sep = ", ") %>%
  select(-source_file.dat2)

# Remove NAs that were added to the rows
replacement_pattern <- c("^NA, " = "",
                         "NA, " = "",
                         ", NA, NA" = "",
                         ", NA" = "")
df_final$source_files <- str_replace_all(df_final$source_files, replacement_pattern)

# Reorganize columns
df_final <- df_final %>% 
  relocate(trawl_date, trawl_location, lake_code, lake_name, process_date, processor,
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

### Combine start_time.sas and .dat into a new 'start_time' column

# First standardize midnight times: instead of 24, represent it as 00
# Remove decimals from .sas time columns
df_final <- df_final %>%
  mutate(across(c(start_time.dat, end_time.dat, start_time.sas, end_time.sas, start_time, end_time), 
                ~ str_replace_all(.x, pattern = "^24", replacement = "00"))) %>%
  mutate(across(c(start_time.sas, end_time.sas), 
                ~ str_replace_all(.x, pattern = ".\\d{5}$", replacement = "")))

## Before combining columns, flag the ones with errors and correct them
# Define the regex pattern for HH:MM:SS format
time_pattern <- "^([01]?[0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]$"

# Use grepl() to create a logical vector indicating valid formats in .dat time
is_valid_dat <- ifelse(is.na(df_final$start_time.dat), NA, grepl(time_pattern, df_final$start_time.dat))
is_valid_sas <- ifelse(is.na(df_final$start_time.sas), NA, grepl(time_pattern, df_final$start_time.sas))
is_valid <- ifelse(is.na(df_final$start_time), NA, grepl(time_pattern, df_final$start_time))

# Add a new column to the data frame to flag invalid entries
#df_final$invalid_start_time <- ifelse(is_valid_sas & is_valid_dat, "Valid", "Invalid format")
df_final <- df_final %>%
  mutate(invalid_start_time = case_when(is_valid_sas == "TRUE" ~ "Valid",
                                        is_valid_dat == "TRUE" ~ "Valid",
                                        is_valid == "TRUE" ~ "Valid",
                                        TRUE ~ "Invalid format"))

# Check number of problematic rows
sum(df_final$invalid_start_time == "Invalid format", na.rm = TRUE)

# Save errors in start and end time in separate document
start_time_errors <- df_final[df_final$invalid_start_time == "Invalid format", na.rm = TRUE]
write.csv(start_time_errors, paste0(error_directory, "/start_time_errors.csv"), row.names = FALSE)

# Manually fix some of the errors identified
#  everything with 0203 - fish_unique_ID == 1997-09-17_69_9_15_7_1_1.35_49
df_final <- df_final %>%
  mutate(
    end_time = ifelse(duration_mi == "0203", "02:03:00", end_time),
    time_comment = ifelse(duration_mi == "0203", "0203 error in duration_mi was end_time - corrected", time_comment),
    duration_mi = ifelse(duration_mi == "0203", "16", duration_mi)
  )

# Summary of start_time with wrong format    
df_final %>%
  filter(invalid_start_time == "Invalid format") %>%
  group_by(start_time, start_time.sas, start_time.dat) %>%
  summarise(count = n()) -> summary_table_time

# Clean the invalid numbers. Replace them by NA.
# start_time column substitution patterns
replacement_pattern <- c("08:65:00" = NA_character_,
                         "14:69:00" = NA_character_,
                         "15:92:00" = NA_character_,
                         "21:89:00" = NA_character_)
df_final <- df_final %>%
  mutate(start_time = str_replace_all(start_time, replacement_pattern)) 

# start_time.sas column substitution patterns
replacement_pattern <- c("99:40:13" = NA_character_, "100:38:24" = NA_character_)
df_final <- df_final %>%
  mutate(start_time.sas = str_replace_all(start_time.sas, replacement_pattern)) 

# start_time.dat column substitution patterns
replacement_pattern <- c("99:99:00" = NA_character_, "00:0\\?:00" = NA_character_)
df_final <- df_final %>%
  mutate(start_time.dat = str_replace_all(start_time.dat, replacement_pattern)) 

### Combine start_time and end_time columns
# First combine start_time.dat and start_time.sas, prioritizing values for the .dat column when both are present
# Then combine everything in the start_time column and delete unnecessary columns
df_final$start_time_combined <- coalesce(df_final$start_time.dat, df_final$start_time.sas)
df_final$start_time_combined <- trimws(df_final$start_time_combined)
df_final$start_time <- coalesce(df_final$start_time, df_final$start_time_combined)
df_final <- df_final %>%
  select(-start_time.dat, -start_time.sas, -start_time_combined) 

# First clean invalid format. Combine end_time.dat and end_time.sas, prioritizing values for the .dat column when both are present
# Then combine everything in the end_time column and delete unnecessary columns
replacement_pattern <- c("NA:NA:00" = NA_character_)
df_final <- df_final %>%
  mutate(end_time = str_replace_all(end_time, replacement_pattern)) 

df_final$end_time_combined <- coalesce(df_final$end_time.dat, df_final$end_time.sas)
df_final$end_time_combined <- trimws(df_final$end_time_combined)
df_final$end_time <- coalesce(df_final$end_time, df_final$end_time_combined)
df_final <- df_final %>%
  select(-end_time.dat, -end_time.sas, -end_time_combined) 

### Separate columns with fish descriptions and common name
# Cleaning the columns
df_final <- df_final %>%
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

df_final <- df_final %>%
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

df_final <- df_final %>%
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

df_final <- df_final %>%
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

df_final <- df_final %>%
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
                         "SCULPIN" = "")

df_final <- df_final %>%
  mutate(species_code_comment = str_replace_all(species_code_comment, replacement_pattern))

### Create Look up table to correct lakes and fish names
# add a column with fish scientific names: genus and species
fish_scientific_name_lookup_table <- data.frame(fish_description = as.character(c("Chinook", "Coho", "Dolly Varden", "Lamprey", "Peamouth Chub", "Pink", "Red-sided Shiner", "Sculpin", "Sockeye", "Stickleback", "Sucker", "Whitefish", "Kokanee")), 
                                                fish_scientific_genus = c("Oncorhynchus", "Oncorhynchus", "Salvelinus", "Lampreta", "Mylocheilus","Oncorhynchus", "Richardsonius", "Cottus", "Oncorhynchus", "Gasterosteus", "Catostomus", "Coregonus", "Oncorhynchus"),
                                                fish_scientific_species = c("tshawytscha", "kisutch", "", "macrostoma", "caurinus", "gorbuscha", "", "", "nerka", "aculeatus", "", "", ""))

# Add a comment for Dolly Varden - taxonomic issues 
df_final$comment <- ifelse(df_final$fish_description == "Dolly Varden", "Salvelinus malma and S. confluentus - might have unsolved taxonomic issues", df_final$comment)

# "species_info_code" = 
# Remove abbreviations in the name of the lakes by importing the lookup table
lake_name <- read.csv("./TRAWL_BIOSAMPLE/00_raw_data/04_YS_look_up_tables/lake_codes.csv")

# Join the tables
df_final <- df_final %>%
  dplyr::left_join(fish_scientific_name_lookup_table, by = "fish_description") %>%
  select(-lake_name) %>%
  dplyr::left_join(lake_name, by = "lake_code")
  
# Save error table with the rows with empty names for fish species.
no_species_record_rows <- df_final[is.na(df_final$fish_description),]
write.csv(no_species_record_rows, paste0(error_directory, "/no_species_record_rows.csv"), row.names = FALSE)

# Delete rows with no species info from the final matrix
df_final <- df_final[!is.na(df_final$fish_description), ]

#### Work on the duration_mi column. Remove "Min" from the column
df_final <- df_final %>%
  mutate(
    ### extract the digits at the start
    duration_mi_clean = str_extract(duration_mi, "^\\d+"),
    ### convert number to integer (optional, based on your needs)
    duration_mi = as.integer(duration_mi_clean)
  ) %>%
  select(-duration_mi_clean)

# Flag problematic rows on duration_mi
df_final <- df_final %>%
  mutate(invalid_duration_time = case_when(duration_mi < 60 ~ "Valid",
                                        TRUE ~ "Invalid format"))
# Check number of problematic rows
sum(df_final$invalid_duration_time == "Invalid format", na.rm = TRUE)

# Save errors in duration in separate document
duration_mi_errors <- df_final[df_final$invalid_duration_time == "Invalid format", na.rm = TRUE]
write.csv(duration_mi_errors, paste0(error_directory, "/duration_mi_errors.csv"), row.names = FALSE)

## In the duration_mi columns, correct values "365" and "535" 
df_final <- df_final %>%
  mutate(duration_mi = as.character(duration_mi)) %>%
  mutate(duration_mi = case_when(duration_mi == "365" ~ NA_character_,
                                 duration_mi == "535" ~ NA_character_,
                                 TRUE ~ duration_mi))
df_final$duration_mi <- as.integer(df_final$duration_mi)

# Replace all "99" and "999" by NA across all columns
df_final <- df_final %>%
  mutate(across(c(duration_mi, processor, depth_m, scale, trawl_number), ~ str_replace_all(.x, pattern = "999", replacement = NA_character_))) %>%
  mutate(across(c(duration_mi, processor, depth_m, trawl_number), ~ str_replace_all(.x, pattern = "99", replacement = NA_character_))) %>%
  mutate(duration_mi = as.integer(duration_mi)) 

## Calculate duration_mi from the difference of the start_time and end_time columns
df_final <- df_final %>%
  mutate(across(c(start_time, end_time), 
                ~ str_replace_all(.x, pattern = "^00", replacement = "24")))
  
# Make sure all rows have the same format
df_final <- df_final %>%
  mutate(start_t = hms(start_time),
         end_t  = hms(end_time)) %>%
  mutate(across(c(start_time, end_time), 
                ~ str_replace_all(.x, pattern = "^24", replacement = "00")))

# Create a column with the calculated duration in minutes
df_final <- df_final %>%
  mutate(duration_from_times = if_else(!is.na(start_t) & !is.na(end_t),
                                       (as.numeric(end_t) - as.numeric(start_t)) / 60, NA_real_))

# Final duration column
df_final <- df_final %>%
  mutate(duration_final = case_when(!is.na(duration_mi) ~ duration_mi,
                                    is.na(duration_mi) & !is.na(duration_from_times) ~ duration_from_times,
                                    TRUE ~ NA_real_),
    duration_from_times = ifelse(duration_from_times < 0, duration_mi, duration_from_times),
    duration_comment = case_when(
      # duration exists and matches times
      !is.na(duration_mi) & !is.na(duration_from_times) &
        abs(duration_mi - duration_from_times) < 1 ~ "unchanged (matches start_time and end_time)",
      # duration exists but does NOT match times
      !is.na(duration_mi) & !is.na(duration_from_times) &
        abs(duration_mi - duration_from_times) >= 1 ~ "unchanged (does NOT match start_time and end_time)",
      # duration calculated from times
      is.na(duration_mi) & !is.na(duration_from_times) ~ "duration not provided, calculated from start_time, end_time",
      # nothing possible
      TRUE ~ "duration could not be calculated"))

df_final %>%
  group_by(start_time, end_time, duration_mi, duration_from_times, duration_comment) %>%
  summarise(count = n()) -> summary_table

hist(df_final$duration_mi)
hist(df_final$duration_from_times)

# Check number of problematic rows
sum(df_final$duration_comment == "unchanged (does NOT match start_time and end_time)", na.rm = TRUE)

# Save mismatches in duration in separate document
duration_mismatch <- df_final[df_final$duration_comment == "unchanged (does NOT match start_time and end_time)", na.rm = TRUE]
write.csv(duration_mismatch, paste0(error_directory, "/duration_mismatch.csv"), row.names = FALSE)

#### Correct errors in preservative_code column
df_final <- df_final %>%
  mutate(preservative_code = as.character(preservative_code)) %>%
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

# Fill up the missing values using a lookup table
preservative_code_lookup_table <- read.csv("./TRAWL_BIOSAMPLE/00_raw_data/04_YS_look_up_tables/preservative_code_lookup_table.csv")
df_final <- rows_patch(df_final, preservative_code_lookup_table, by = "preservative_code", unmatched = "ignore")

# Check if preservative_description match preservative_code
df_final %>%
  group_by(preservative_code, preservative_description, preservative_code_comment) %>%
  summarise(count = n()) -> summary_table

# Convert Trawl_number to integer instead of character
df_final$trawl_number <- as.integer(df_final$trawl_number)

### In the depth_m columns, correct values "98.109375", "14.83203125"
df_final <- df_final %>%
  mutate(depth_m = case_when(depth_m == "98.109375" ~ NA_character_,
                             depth_m == "14.83203125" ~ NA_character_,
                              TRUE ~ depth_m))

# Flag any value greater than "100" and add the comment to a new column
df_final <- df_final %>%
  mutate(depth_m = as.numeric(depth_m)) %>%
  mutate(depth_m_flag = case_when(depth_m > 100 ~ "Depth > 100m! Possible error", 
                                  TRUE ~ NA_character_)) %>%
  unite("depth_m_comments", depth_m_flag, depth_m_comment, sep = ", ", na.rm = TRUE)

### Clean scale, scale_book and scale_book_letter columns
# Replace wrong data scale_book data for the correct number
df_final <- df_final %>%
  mutate(scale_book = case_when(scale_book == "1.86" ~ "0",
                                scale_book == "0.49" ~ "0",
                                scale_book == "0.21" ~ "0",
                                 TRUE ~ scale_book))

# Transfer scale book letters from scale_book column to scale_book_letter
df_final <- df_final %>%
  # Detect if there is a letter in the row and remove them
  mutate(
    scale_book_is_letter = str_detect(scale_book, "^[A-Za-z]$"),
    scale_book_flag_letter = if_else(scale_book_is_letter, scale_book, NA_character_),
    scale_book = if_else(!scale_book_is_letter, scale_book, NA_character_)) %>%
  # Compare if the letters are the same in the different columns - All rows match
  mutate(
    letters_match = case_when(is.na(scale_book_flag_letter) | is.na(scale_book_letter) ~ NA,
                              scale_book_flag_letter == scale_book_letter ~ TRUE,
                              TRUE ~ FALSE)) %>%
  # Replace missing data in the scale_book_letter by values from scale_book column
  mutate(
    scale_book_letter = if_else(is.na(scale_book_letter) & scale_book_is_letter, scale_book_flag_letter, scale_book_letter)) %>%
  # Delete extra rows
  select(-scale_book_flag_letter, -scale_book_flag_letter, -scale_book_is_letter, -letters_match)

# Create a new column for the scale_book columns, scale_book_comment
df_final <- df_final %>%
  mutate(
    ### extract the digits at the start
    scale_book_clean = str_extract(scale_book, "^\\w{1,3}"),
    
    ### extract the rest of the string as comment
    scale_book_comment = str_trim(str_remove(scale_book, "^\\d{1,4}")),
    
    ### replace missing/empty comments with ""
    scale_book_comment = if_else(scale_book_comment == "" | is.na(scale_book_comment),
                                 NA_character_, scale_book_comment),

    ### convert number to integer (optional, based on your needs)
    scale_book = as.integer(scale_book_clean)) %>%
  select(-scale_book_clean)

### Remove values greater than 20 and add a comment in the scale_book_comment column
df_final <- df_final  %>% 
  mutate(scale_book_comment2 = case_when(scale_book > 20 ~ "scale_book > 20, possible error, deleted",
                               TRUE ~ scale_book_comment),
         scale_book = case_when(scale_book > 20 ~ NA_real_,
                       TRUE ~ scale_book),
         scale_book_comment = scale_book_comment2) %>% 
  select(-scale_book_comment2)

# Add comment for the corrected rows 
rows = c(93221, 92368, 93251)
df_final <- df_final %>%
  mutate(scale_book_comment = if_else(row_number() %in% rows,
                                      if_else(is.na(scale_book_comment) | scale_book_comment == "",
                                              "Wrong weight entry replaced with scale_book code 0", 
                                              paste(scale_book_comment, "Wrong weight entry replaced with scale_book code 0", sep = "; ")),
                                      scale_book_comment))

df_final %>%
  group_by(scale, scale_book, scale_book_letter, scale_book_comment) %>%
  summarise(count = n()) -> summary_table

# Combine all comments in a general_comments column, skiping the column when it is NA
df_final <- df_final %>%
rowwise() %>%
  mutate(general_comments = paste(
      c(if (!is.na(trawl_location) & trawl_location != "") paste0("trawl_location: ", trawl_location),
        if (!is.na(comment) & comment != "") paste0("comment: ", comment),
        if (!is.na(time_comment) & time_comment != "") paste0("time_comment: ", time_comment),
        if (!is.na(duration_comment) & duration_comment != "") paste0("duration_comment: ", duration_comment),
        if (!is.na(preservative_code_comment) & preservative_code_comment != "") paste0("preservative_code_comment: ", preservative_code_comment),
        if (!is.na(depth_m_comments) & depth_m_comments != "") paste0("depth_m_comments: ", depth_m_comments),
        if (!is.na(trawl_date_comment) & trawl_date_comment != "") paste0("trawl_date_comment: ", trawl_date_comment),
        if (!is.na(trawl_number_comment) & trawl_number_comment != "") paste0("trawl_number_comment: ", trawl_number_comment),
        if (!is.na(merging_update_type) & merging_update_type != "") paste0("merging_update_type: ", merging_update_type),
        if (!is.na(scale_book_comment) & scale_book_comment != "") paste0("scale_book_comment: ", scale_book_comment),
        if (!is.na(invalid_duration_time) & invalid_duration_time != "") paste0("invalid_duration_time: ", invalid_duration_time),
        if (!is.na(invalid_start_time) & invalid_start_time != "") paste0("invalid_start_time: ", invalid_start_time)),
      collapse = "; "
    )) %>%
  ungroup()  %>%
  select(-trawl_location, -comment, -time_comment, -preservative_code_comment, -depth_m_comments, 
        -trawl_date_comment, -trawl_number_comment, -merging_update_type, -scale_book_comment,
        -duration_comment, -invalid_duration_time, -start_t, -end_t, -invalid_start_time)

# Reorganize and rename columns
df_final <- df_final %>% 
  relocate(trawl_date, lake_code, lake_name, lake_latitude, lake_longitude, process_date, processor,
           start_time, end_time, "duration_minutes" = duration_mi, depth_m, trawl_number, sample_type, species_code, "species_common_name" = fish_description, 
           fish_scientific_genus, fish_scientific_species, "life_stage" = species_code_comment, "age_class" = age, 
           aging_technique, aging_technique_name, fish_length_mm, fish_weight_g, 
           weight_conversion_formula, standardized_weight_g, fish_total, fish_id, 
           preservative_code, preservative_description, sample_number, scale, scale_book, scale_book_letter, 
           trawl_unique_ID, fish_unique_ID, source_files, source_line, 
           trawl_month, ats_year, general_comments) %>%
  select(-.joyn, -program_notes)

# Save document until here
write.csv(df_final, paste0(working_directory, "/combined_inprogress_df_trawl.csv"), row.names = FALSE)



######## in progress #############

#paste(df_final$fish_description, df_final$species_code_comment, sep = " / ") 
df_final %>%
  group_by(species_code, fish_description, species_code_comment) %>%
  summarise(count = n()) -> summary_table

#paste(df_final$fish_description, df_final$species_code_comment, sep = " / ") 
df_final %>%
  group_by(fish_description, species_code_comment) %>%
  summarise(count = n()) -> summary_table

unique(df_final$duration_mi)

unique(final_df_try$duration_mi)
df_final <- as.data.frame(df_final)
df_final[df_final$duration_mi == "535.94091796875", ]

### Example of conflicting rows to test
# Before concatenation of rows
df_joined[df_joined$fish_unique_ID == "1984-06-26_40_6_6_2_1_2.52_63", ]# seems duplicates, but it isn't
df_joined[df_joined$fish_unique_ID == "1984-06-26_40_6_6_2_3_2.09_59", ]
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
df_final[df_final$fish_unique_ID == "1996-07-24_2_6_14_32_1_47.98_160", ]
df_final[df_final$fish_unique_ID == "1984-05-01_40_1_2_2_6_0.22_32", ]
df_final[df_final$fish_unique_ID == "1984-04-25_6_1_4_1_1_3.12_67", ]



unique(sas_total_fish_error$fish_description)

df_sas_total_fish_error <- as.data.frame(df_sas_total_fish_error)
df_sas_total_fish_error[df_sas_total_fish_error$fish_unique_ID == "1984-05-29_40_1_0_2_1_1.4_54", ]
df_joined[df_joined$fish_unique_ID == "1984-05-29_40_1_0_2_1_1.4_54", ]

sas_total_fish_error[sas_total_fish_error$fish_unique_ID == "1995-10-03_16_14_22_6_1_1.94_56", ]
non_unique_data[non_unique_data$fish_unique_ID == "1984-04-25_6_1_4_1_1_3.12_67", ]
sas_total_fish_error[sas_total_fish_error$fish_unique_ID == "1984-04-25_6_1_4_1_1_3.12_67", ]

df_final <- as.data.frame(df_final)

df_final[df_final$fish_unique_ID == "1986-04-16_161_4_15_1_4_3.73_68", ]
df_final[df_final$fish_unique_ID == "1995-10-03_16_14_22_6_1_1.94_56", ]



