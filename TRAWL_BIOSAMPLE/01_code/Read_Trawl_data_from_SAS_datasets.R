###############################################################################
##############            TRAWL data code 1977-1999         ###################
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

# Regex pattern to automate file processing (loop over all SAS files)
name_pattern <- "trawl([[:digit:]][[:digit:]])\\.sas7bdat$"
data_folder <-"./TRAWL_BIOSAMPLE/00_raw_data/02_SAS_Data"
intermediate_out_folder <- "./TRAWL_BIOSAMPLE/02_intermediate_out"

# Created a function to test the regex. Uncomment it if you'd like to use.
#see <- function(rx) str_view_all("trawl92.sas7bdat", rx)
#see("trawl([[:digit:]][[:digit:]])\\.sas7bdat$")

files <- list.files(data_folder, pattern = name_pattern, full.names = TRUE)

# Remove corrupted files, because their metadata have some structural problem
remove_yy <- c("trawl85.sas7bdat", 
               "trawl86.sas7bdat",
               "trawl87.sas7bdat",
               "trawl94.sas7bdat",
               "trawl97.sas7bdat", 
               "trawl98.sas7bdat")
files <- files[!basename(files) %in% remove_yy]

cat("Found", length(files), "files:\n")
print(files)

################################  Step 2  #####################################
################### Read and clean the SAS data files #########################

# Loop over SAS files in the raw folder, combine with metadata and save them in csv
for (f in files) {
  cat("\nprocessing file:", f, "\n")
  
  trawlyy <- read_sas(f, NULL)
  metadata_yy <- sub(name_pattern, "\\1", basename(f))
  metadata_trawl <- read_sas(file.path(data_folder, paste0("trlinf", metadata_yy, ".sas7bdat")))
  
  metadata_trawl <- metadata_trawl %>%
    mutate(
      nmbrfish = as.integer(nmbrfish)
    )
  
  # Combine the metadata matrix with trawl information
  # Some rows have no observer, so it is better to not use them as a matching column
  final_df <- full_join(metadata_trawl, trawlyy, by = c("date", "trwlnmbr", "system", "depth", "fspecies"))
  
  # Cleaning columns' names
  rename_map <- c("process_date" = "procdate", 
                "processor" = "observer.x", 
                "processor" = "observer",
                "lake_code" = "system", 
                "trawl_date" = "date", 
                "sample_number" = "sample", 
                "trawl_number" = "trwlnmbr", 
                "start_time" = "statime", 
                "end_time" = "endtime",
                "duration_mi" = "duration", 
                "depth_m" = "depth", 
                "species_code" = "fspecies",
                "aging_technique" = "aged", 
                "preservative_code" = "prsrv", 
                "fish_length_mm" = "length",
                "fish_weight_g" = "weight",
                "standardized_weight_g" = "stdwght",
                "scale_book" = "sclbook",
                "fish_id" = "fishnmbr",
                "fish_total" = "nmbrfish")
  
  # Match with the columns present in the dataframe
  rename_map <- rename_map[rename_map %in% names(final_df)]
  # Rename columns
  final_df <- final_df %>%  
    rename(!!!rename_map)
  # Remove duplicated column "observer.y", present in some files
  if ("observer.y" %in% names(final_df)){
    final_df$observer.y <- NULL
    message("Removed column 'observer.y' from table")
    
  }
    
  ### save table in csv 
  write.csv(final_df, paste0(intermediate_out_folder,"/trawl", metadata_yy, "_SAS.csv"), row.names = FALSE)


################################  Step 3  #####################################
################### Create look up tables to organize data ####################

  final_df <- read.csv(paste0(intermediate_out_folder,"/trawl", metadata_yy, "_SAS.csv"))
  
    # check and add new columns to standardize the columns in the spreadsheet
    new_columns <- c("process_date", "sample_number", 
                    "scale", "scale_book",
                    "age", "aging_technique", "species_code_comment")
    
    for (columns in new_columns) {
      if (!columns %in% names(final_df)) {
        final_df[[columns]] <- NA 
    }
  }
  ### load your look-up tables
  fish_species_code_lookup_table <- read.csv("./TRAWL_BIOSAMPLE/00_raw_data/04_YS_look_up_tables/fish_species_code_lookup_table.csv")
  
  preservative_code_lookup_table <- read.csv("./TRAWL_BIOSAMPLE/00_raw_data/04_YS_look_up_tables/preservative_code_lookup_table.csv") 
  
  preservative_code_weight_conversion_lookup_table <- read.csv("./TRAWL_BIOSAMPLE/00_raw_data/04_YS_look_up_tables/preservative_code_weight_conv_lookup_table.csv")
  
  lake_name_lookup_table <- read.csv("./TRAWL_BIOSAMPLE/00_raw_data/04_YS_look_up_tables/lake_name_table.csv")
  
  aging_technique_lookup_table <- data.frame(aging_technique = as.integer(c(0, 1, 2, 3, 4, 5)), 
                                             aging_technique_name = c("No scale", "Scale & aged", "Scale - Poor", "Scale - Regen.", 
                                                   "Scale - Unsure", "Defaulted"))
  
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
  
  preservative_code_weight_conversion_lookup_table <- preservative_code_weight_conversion_lookup_table %>%
    mutate(preservative_code = as.character(preservative_code))
  
  lake_name_lookup_table <- lake_name_lookup_table %>%
    mutate(lake_code = as.character(lake_code))
  
  ### join the tables
  final_df <- final_df %>%
    left_join(fish_species_code_lookup_table, by = "species_code") %>%
    left_join(preservative_code_lookup_table, by = "preservative_code") %>%
    left_join(preservative_code_weight_conversion_lookup_table, by = "preservative_code") %>%
    left_join(lake_name_lookup_table, by = "lake_code") %>%
    left_join(aging_technique_lookup_table, by = "aging_technique")
  
  ### processor column removing comments
  final_df <- final_df %>%
    mutate(
      ### extract the digits at the start
      processor_clean = str_extract(processor, "^\\d{1,2}"),
      ### convert number to integer (optional, based on your needs)
      processor = as.integer(processor_clean)
    ) %>%
    select(-processor_clean)
  
  ### add a source file 
  final_df <- final_df %>%
    mutate(source_file = paste0("trawl", metadata_yy, ".sas7bdat"))
  
  ### adding a column and populating with my info 
  final_df <- final_df %>%
    mutate(program_notes = "AA - Living Data Program - 2025")
  
  ### Round the columns
  final_df$standardized_weight_g <- round(final_df$standardized_weight_g, digits = 2)
  final_df$fish_weight_g <- round(final_df$fish_weight_g, digits = 2)
  final_df$fish_length_mm <- round(final_df$fish_length_mm, digits = 2)
  
  ### Convert the types of data match
  final_df <- final_df %>%
    mutate(
      #scale = as.logical(scale),
      scale_book = as.character(scale_book),
      #age = as.logical(age),
      #aging_technique = as.logical(aging_technique),
      #aging_technique_name = as.logical(aging_technique_name),
      species_code_comment = as.character(species_code_comment),
      duration_mi = as.character(duration_mi)
    )
  
  ### Creating unique IDs for fishes and Trawls, actually avoiding duplicates
  final_df$trawl_unique_ID <- paste(final_df$trawl_date, final_df$lake_code, final_df$trawl_number, final_df$depth_m, sep = "_")
  final_df$fish_unique_ID <- paste(final_df$trawl_date, final_df$lake_code, final_df$trawl_number, final_df$depth_m, final_df$species_code, final_df$fish_id, final_df$fish_weight_g, final_df$fish_length_mm, sep = "_")
  
  # Reorganize columns order
  final_df <- final_df %>% 
    relocate(process_date, processor,  
             lake_code, lake_name = lake, trawl_date,
             start_time, end_time, duration_mi, depth_m,  
             trawl_number, fish_total, fish_id, species_code, fish_description,
             fish_length_mm, fish_weight_g, weight_conversion_formula, 
             preservative_code, preservative_description, sample_number, 
             scale, scale_book, age, aging_technique, aging_technique_name, source_file)
  
  ### adding ats_year and trawl_month
  final_df <- final_df %>%
    mutate(
      trawl_date = ymd(trawl_date),
      trawl_month = month(trawl_date),
      ats_year = year(trawl_date) - if_else(trawl_month < 4, 1L, 0L)
    )
  
  ### Checking for duplicates
  # Check for duplicates based on selected columns
  duplicated_rows <- final_df[duplicated(final_df[ , !names(final_df) %in% "source_line"]), ]
  
  all_duplicates <- final_df %>%
    group_by(fish_unique_ID, species_code_comment) %>%
    filter(n() > 1) %>%
    arrange(fish_unique_ID) %>%
    ungroup()
  
  n_removed <- nrow(duplicated_rows)
  cat("Removed", n_removed, "exact duplicate rows across all columns\n")
  
  # Remove duplicates
  final_df <- final_df[!duplicated(final_df[ , !names(final_df) %in% "source_line"]), ]
  
  ### save table in csv 
  write.csv(final_df, paste0(intermediate_out_folder,"/trawl", metadata_yy, "_SAS.csv"), row.names = FALSE)
}








