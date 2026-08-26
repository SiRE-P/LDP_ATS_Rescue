###############################################################################
##############                  TRAWL data                  ###################
##############            Run all other R scripts           ###################
##############  Authors: Alice Assmar (McGill Uni.), David  ###################
##############        Hunt (McGill Uni.), Howard Stiff      ###################
##############  (DFO Nanaimo), Athena Ogden (DFO Nanaimo)   ###################
###############################################################################

# ----------------------------------------
# 00_run_pipeline.R
# This script orchestrates the running of other R scripts.
# ----------------------------------------

# getwd()
# setwd("./LDP_ATS_Rescue")
{
print("Starting main script execution...")
start_time <- Sys.time() # get time-stamp to indicate start time

################        Activate package libraries      ########################

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
}

################################  Step 1  #####################################
################          Organize the directory       ########################

## Renaming existing directory if it is not standardized
## Careful as this command can delete the current open script. Save the script again.

#if (!dir.exists("./TRAWL_BIOSAMPLE/01_code")) {
#  if(dir.exists("./TRAWL_BIOSAMPLE/code")) {
#    renaming_folder <- file.rename("./TRAWL_BIOSAMPLE/code", "./TRAWL_BIOSAMPLE/01_code")
#    if (renaming_folder){
#      message("Directory renamed successfully.") 
#    } else {
#      warning("Failed to rename directory")
#    }
#  } else {
#    warning("Old directory does not exist.")
#  }
#} else {
#  message("New directory already exists.")
#}
#
## Renaming raw data folder
#if (!dir.exists("./TRAWL_BIOSAMPLE/00_raw_data")) {
#  if(dir.exists("./TRAWL_BIOSAMPLE/data")) {
#    renaming_folder <- file.rename("./TRAWL_BIOSAMPLE/data", "./TRAWL_BIOSAMPLE/00_raw_data")
#    if (renaming_folder){
#      message("Directory renamed successfully.") 
#    } else {
#      warning("Failed to rename directory")
#    }
#  } else {
#    warning("Old directory does not exist.")
#  }
#} else {
#  message("New directory already exists.")
#}

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


################################  Step 2  #####################################
################ Read and convert .dat and .sas to CSV ########################

# Define a function to safely source scripts and handle potential errors
source_script <- function(file_path) {
  if (file.exists(file_path)) {
    message(paste("--- Running", basename(file_path), "---"))
    # Use source() to execute the other R scripts
    source(file_path, echo = FALSE)
    message(paste("--- Finished", basename(file_path), "---"))
  } else {
    warning(paste("Script not found:", file_path))
  }
}

## Run scripts: Data recovery
# Recover .sas files and save them in .csv. in the intermediate_out folder
cat("Converting raw SAS data files to CSV format...\n")
source_script("./TRAWL_BIOSAMPLE/01_code/Read_Trawl_data_from_SAS_datasets.R")

cat("Succesfully finished task...\n")

# Recover .dat files and save them in .csv  in the intermediate_out folder
# Trawl years 1984 and 1985
cat("Converting raw DAT data files to CSV format...\n")

source_script("./TRAWL_BIOSAMPLE/01_code/Read_Trawl_data_from_DAT_datasets_84-86.R")

# Trawl years 1986
source_script("./TRAWL_BIOSAMPLE/01_code/Read_Trawl_data_from_DAT_datasets_86.R")

# Trawl years 1987
source_script("./TRAWL_BIOSAMPLE/01_code/Read_Trawl_data_from_DAT_datasets_87.R")

# Trawl years 1988, '90, '91, '93, '94, '95, '96, '97
source_script("./TRAWL_BIOSAMPLE/01_code/Read_Trawl_data_from_DAT_datasets_88-99.R")

# Trawl years 1989, '92 and '98
source_script("./TRAWL_BIOSAMPLE/01_code/Read_Trawl_data_from_DAT_datasets_92_98.R")

# Trawl years 1999
source_script("./TRAWL_BIOSAMPLE/01_code/Read_Trawl_data_from_DAT_datasets_99.R")

cat("Succesfully finished task...\n")

################################  Step 3  #####################################
###################    Data summary before assembly   #########################

# Run script 2: Summary table
source_script("./TRAWL_BIOSAMPLE/01_code/summary_table.R")

################################  Step 4  #####################################
###################    Data Cleaning and Assembly   #########################

# Run final script: Data Cleaning and super matrix assembly
print("Running final script: Assemble_csv_dat_sas.R")
# Source the third script
tryCatch({
  source("./TRAWL_BIOSAMPLE/01_code/Assemble_csv_dat_sas.R")
  print("Assemble_csv_dat_sas.R finished successfully.")
}, error = function(e) {
  print(paste("Error in Assemble_csv_dat_sas.R:", e$message))
})

###################     Finished     #########################

end_time <- Sys.time() # get time-stamp to indicate end time
time_spent_min <- as.numeric(difftime(end_time, start_time, units = "mins"))

message("\nAll scripts have been executed.")

cat(
  "Script started at:", format(start_time, "%d-%b-%Y %H:%M"), "\n",
  "Script ended at:  ", format(end_time,   "%d-%b-%Y %H:%M"), "\n",
  "Total runtime:    ", round(time_spent_min, 2), "minutes\n"
)
}
