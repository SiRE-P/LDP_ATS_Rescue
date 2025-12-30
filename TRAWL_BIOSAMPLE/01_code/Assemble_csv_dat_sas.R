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

# Save the erros in the  until here

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

df_joined <- as.data.frame(df_joined)

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

# Is there still duplicates? There are some rows with equal fish_unique_id but are not the same record
all_duplicates <- df_final %>%
  group_by(fish_unique_ID) %>%
  filter(n() > 1) %>%
  ungroup()

# Save duplicated rows until here
write.csv(all_duplicates, paste0(error_directory, "/duplicated_df_trawl.csv"), row.names = FALSE)


### Combine start_time.sas and .dat into a new 'start_time' column - .sas files have the time set better formatted

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
write.csv(df_final, paste0(error_directory, "/start_time_errors.csv"), row.names = FALSE)

###
### Combine start_time and end_time columns


###

### Separate columns with fish descriptions and common name
# Cleaning the columns
df_final <- df_final %>%
  # compare fish_description and species_code_comments columns
  mutate(
    ### Create a column stating the matches
    species_code_comment_match = (tolower(df_final$species_code_comment) == tolower(df_final$fish_description)),
    ### Delete the duplicates from the species_code_comment
    species_code_comment = ifelse(species_code_comment_match == "TRUE", "", species_code_comment)
    ) %>%
  # Fish description column removing comments and placing into separate column 
  mutate(
    ### extract the description at the end
    fish_description_clean = str_extract(fish_description, "\\s\\(\\w+\\)$"),
    
    ### extract the rest of the string as the common name of the fish
    fish_description = str_trim(str_remove(fish_description, "\\s\\(\\w+\\)$")),
    
    ### Save the stage description in the fish_description column
    fish_name = as.character(fish_description_clean)
    ) %>%
  unite(species_code_comment, species_code_comment, fish_name, sep = ",") %>%
  ### extract the description at the end
  select(-species_code_comment_match, -fish_description_clean)

# Remove NAs that were added to the rows
replacement_pattern <- c("NA, " = "",
                         ",NA" = "",
                         "NA,NA" = "",
                         "NA" = "",
                         "^, \\(" = "\\(")
  
df_final$species_code_comment <- str_replace_all(df_final$species_code_comment, replacement_pattern)

#### Work on the duration_mi column. Remove "Min" in the duration column
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
write.csv(df_final, paste0(error_directory, "/duration_mi_errors.csv"), row.names = FALSE)


unique(df_joined$duration_mi)
unique(df_final$duration_mi)
df_final <- as.data.frame(df_final)
df_joined <- as.data.frame(df_joined)
df_final[df_final$fish_unique_ID == "1991-09-14_66_99_10_7_1_0.72_43",]
df_joined[df_joined$fish_unique_ID == "1991-09-14_66_99_10_7_1_0.72_43",]

unique(df_joined$species_code_comment)
df_joined[df_joined$species_code_comment == "STICKLEBACK GRAVID FEMALE",]

unique(paste(df_final$fish_description, df_final$species_code_comment, sep = " / "))

#paste(df_final$fish_description, df_final$species_code_comment, sep = " / ") %>%
df_final %>%
  group_by(fish_description, species_code_comment) %>%
  summarise(count = n()) -> summary_table


#### Work on the preservative_code columun.
unique(df_final$preservative_code)
unique(df_joined$preservative_code)
df_final <- as.data.frame(df_final)
df_final[df_final$preservative_code == "270", ]


#  preservative_code	preservative_description
#  0	Fresh
#  1	Formalin/5weeks
#  2	70% ETOH/5weeks
#  3	50% ETOH/5weeks
#  4	Formalin/>5months
#  9	Unknown
#  95	Formalin/1-2days
#  96	Formalin/3weeks
#  97	Formalin/4weeks
#  98	Unknown
#  99	Formalin/Assumed



######## in progress #############
# Save document until here
write.csv(df_final, paste0(working_directory, "/combined_inprogress_df_trawl.csv"), row.names = FALSE)






unique(df_final$duration_mi)

unique(final_df_try$duration_mi)
df_final <- as.data.frame(df_final)
df_final[df_final$duration_mi == "535.94091796875", ]



### Correct .dat column time with invalid format, and merge start_time.dat/.sas and end_time.dat/.sas

df <- df_final
is_valid_sas <- grepl(time_pattern, df_final$start_time.sas)
df <- df %>%
  mutate(
    ### replace the invalid values with the .sas info
    start_time.dat = if_else(is_valid_sas & invalid_start_time == "Invalid format",
                             start_time.dat,
                             start_time.sas))

### Drop unnecessary columns and reorganize column order.

# Save combined data frame
write.csv(df_final, paste0(working_directory, "/combined_df_trawl.csv"), row.names = FALSE)



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
df_final[df_final$fish_unique_ID == "1984-06-26_40_6_6_2_1_2.52_63", ]
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


