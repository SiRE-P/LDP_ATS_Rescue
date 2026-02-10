###############################################################################
##############                  TRAWL data                  ###################
##############     summarizing SAS and .dat trawl files     ###################
############## Authors: Alice Assmar (McGill Uni.), David   ###################
##############             Hunt (McGill Uni.),              ###################
##############          Howard Stiff (DFO Nanaimo),         ###################
##############        Athena Ogden (DFO Nanaimo)            ###################
###############################################################################

# getwd()
# setwd("./LDP_ATS_Rescue")

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
  library(tidyverse)
  library(tools)
  library(Rcpp) 
  library(haven) # read SAS files
}

###############
#### Read raw tables if necessary
#trawlyy_sas <- read_sas("./TRAWL_BIOSAMPLE/00_raw_data/02_SAS_Data/trawl87.sas7bdat", NULL)
#metadata_trawl_yy <- read_sas("./TRAWL_BIOSAMPLE/00_raw_data/02_SAS_Data/trlinf87.sas7bdat", NULL)

#### Read csv tables
intermediate_out_folder <- "./TRAWL_BIOSAMPLE/02_intermediate_out"
#trawl_file <- "Trawl96"
trawl_files <- c("trawl84", "trawl88", "trawl89", "trawl90", "trawl91", "trawl92", "trawl93", "trawl95", "trawl96")

for (trawl_file in trawl_files) {
  
  cat("Processing", trawl_file, "\n")

final_csv_dat <- read.csv(paste0(intermediate_out_folder,"/", trawl_file, "_DAT.csv"))
final_csv_sas <- read.csv(paste0(intermediate_out_folder,"/", trawl_file, "_SAS.csv"))

#### Summarize dat and SAS tables
summary(final_csv_dat)
summary(final_csv_sas)
# How many dates the lake was surveyed
aggregate(trawl_date ~ lake_code, data = final_csv_dat, FUN = function(x) length(unique(x)))
aggregate(trawl_date ~ lake_code, data = final_csv_sas, FUN = function(x) length(unique(x)))

# How many dates the lake was surveyed and number of trawls
aggregate(cbind(trawl_date, trawl_number) ~ lake_code, data = final_csv_dat, FUN = function(x) length(unique(x)))
aggregate(cbind(trawl_date, trawl_number) ~ lake_code, data = final_csv_sas, FUN = function(x) length(unique(x)))

# Check how many columns match
# create a data set with the info I want to check
df_dat <- tibble(unique = unique(paste(final_csv_dat$fish_unique_ID, final_csv_dat$lake_code, final_csv_dat$trawl_number, final_csv_dat$trawl_date, final_csv_dat$fish_weight_g, final_csv_dat$duration_mi))) 
df_sas <- tibble(unique = unique(paste(final_csv_sas$fish_unique_ID, final_csv_sas$lake_code, final_csv_sas$trawl_number, final_csv_sas$trawl_date, final_csv_sas$fish_weight_g, final_csv_sas$duration_mi)))

# create a data set with the info I want to check
#df_dat <- tibble(unique = paste(final_csv_dat$fish_unique_ID, final_csv_dat$lake_code, final_csv_dat$trawl_number, final_csv_dat$trawl_date, final_csv_dat$fish_id)) 
#df_sas <- tibble(unique = paste(final_csv_sas$fish_unique_ID, final_csv_sas$lake_code, final_csv_sas$trawl_number, final_csv_sas$trawl_date, final_csv_sas$fish_id))


df_dat <- df_dat %>%
  separate(col = unique, into = c("fish_unique_ID","lake_code", "trawl_number","trawl_date", "fish_weight_g", "duration_mi"), sep = " ")

df_sas <- df_sas %>%
  separate(col = unique, into = c("fish_unique_ID", "lake_code", "trawl_number", "trawl_date", "fish_weight_g", "duration_mi"), sep = " ")

# line up identical rows
columns <- c("lake_code", "trawl_number", "trawl_date", "fish_weight_g", "duration_mi")

# Check for leading/trailing whitespace
for (col in columns) {
  df_sas[[col]] <- trimws(df_sas[[col]])
}

for (col in columns) {
  df_dat[[col]] <- trimws(df_dat[[col]])
}

# Merge the database using the unique ID to combine
merged_df <- merge(df_sas [, c("fish_unique_ID", columns), drop = FALSE],
                   df_dat [, c("fish_unique_ID", columns), drop = FALSE],
                   by = "fish_unique_ID",
                   all = TRUE, # Keep all
                   suffixes = c(".sas", ".dat"))

# Compare the columns
# store the names of the columns
cols_sas <- paste0(columns,".sas")
cols_dat <- paste0(columns,".dat")

# Check if they have the same class
sapply(merged_df[cols_sas], class)
sapply(merged_df[cols_dat], class)


# Create a match/not match column
merged_df <- merged_df %>%
  rowwise() %>%
  mutate(
    match = {
      left  <- c_across(paste0(cols_sas))
      right <- c_across(paste0(cols_dat))
      
      # compare the rows
      compare_rows <- mapply(function(x, y) {
        if (is.na(x) & is.na(y)) TRUE
        else if (is.na(x) | is.na(y)) FALSE
        else x ==y
      }, left, right)
      # Check if all match, label as "MATCH" or " NOT MATCH"
      if (all(compare_rows)) "MATCH" else "NOT MATCH"
    }
  ) %>%
  ungroup()

# Save final table in csv
write.csv(merged_df, paste0(intermediate_out_folder, "/", trawl_file, "_inventory.csv"), row.names = FALSE)
}
## Count number of match
#sum(merged_df$match == "MATCH", na.rm = TRUE)
#sum(merged_df$match == "NOT MATCH", na.rm = TRUE)
#
## Check length for unique lakes and which lakes are present
#length(unique(final_csv_dat$lake_code))
#unique(final_csv_dat$lake_code)
#
#length(unique(final_csv_sas$lake_code))
#unique(final_csv_sas$lake_code)
#
#unique(paste(final_csv_dat$lake_code, final_csv_dat$trawl_date))
#unique(paste(final_csv_sas$lake_code, final_csv_sas$trawl_date))
#
## Read inventory
#final_csv_inventory <- read.csv(paste0(intermediate_out_folder,"/", trawl_file, "_inventory.csv"))

