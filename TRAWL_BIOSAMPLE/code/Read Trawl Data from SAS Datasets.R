## --------------------------------------------------------------------------
## FILENAME.R
##
## Title:   
## Purpose: 
## Author:  H Stiff
## Date:    20.
## Notes:   
##          
## --------------------------------------------------------------------------
# install.packages("gt", dependencies = TRUE)
library(dplyr)
library(gt)
library(haven)
library(janitor)
library(readr)
library(stringr)



# Set year
yy <- "77"

# Define file paths
input_path <- file.path("C:/Users/StiffH/Documents/FISHERIES/SALMON INDEX STOCKS/Trawl/TrawlData/SASData")
output_file <- paste0("trwl", yy, ".pc.dat")

# Read the metadata 
meta_file  <- file.path(input_path, paste0("trlinf", yy, ".sas7bdat"))  # Adjust extension if needed
trawl_info <- read_sas(meta_file) 

# Add a label to columns
trawl_info$trwlnmbr <- labelled(trawl_info$trwlnmbr, label = "Trawl")
trawl_info$comment  <- labelled(trawl_info$comment,  label = "Comment")
# trawl_info$statime  <- labelled(trawl_info$statime,  label = "Start")
# trawl_info$endtime  <- labelled(trawl_info$endtime,  label = "Finish")
trawl_info$nmbrfish <- labelled(trawl_info$nmbrfish, label = "Fish")

# Read the trawl data
data_file  <- file.path(input_path, paste0("trawl", yy, ".sas7bdat"))  # Adjust extension if needed
trawl_data <- read_sas(data_file)

# Apply formats (assuming you have lookup tables or named vectors for formats)
# Example format mappings (replace with actual mappings)
sysfmt <- c("35" = "System A", "38" = "System B", "43" = "System C")
fspfmt <- c("101" = "Species X", "102" = "Species Y")

trawl_info %>%
  gt() %>%
  tab_header(
    title = paste0("19", yy, " Trawl Meta-Data Table")) %>%
  cols_label(
    statime = "Start",
    endtime = "Finish") %>%
  fmt_number(
    columns = c(depth, duration, nmbrfish),
    decimals = 0) %>%
  fmt_date(
    columns = date,
    date_style = "y.mn.day") 

# Add formatted columns
trawl_data <- trawl_data %>%
  mutate(
    sysname = ifelse(!is.na(system),   sysfmt[as.character(system)],   NA),
    fspname = ifelse(!is.na(fspecies), fspfmt[as.character(fspecies)], NA))

# # Write to output file
# write_delim(df, output_file, delim = "\t")

# Summary table similar to PROC TABULATE
summary_table <- trawl_data %>%
  group_by(sysname, date, fspname, trwlnmbr) %>%
  summarise(
    count = n(),
    mean_length  = mean(length, na.rm = TRUE),
    sd_length    = sd(length, na.rm = TRUE),
    min_length   = min(length, na.rm = TRUE),
    max_length   = max(length, na.rm = TRUE),
    mean_weight  = mean(weight, na.rm = TRUE),
    sd_weight    = sd(weight, na.rm = TRUE),
    min_weight   = min(weight, na.rm = TRUE),
    max_weight   = max(weight, na.rm = TRUE),
    mean_stdwght = mean(stdwght, na.rm = TRUE),
    sd_stdwght   = sd(stdwght, na.rm = TRUE),
    min_stdwght  = min(stdwght, na.rm = TRUE),
    max_stdwght  = max(stdwght, na.rm = TRUE),
    .groups = "drop")

# Display with gt
summary_table %>%
  gt() %>%
  tab_header(
    title = paste0("19", yy, " Trawl Sample Data")
  ) %>%
  fmt_number(
    columns = c(mean_length,  sd_length,  min_length,  max_length,
                mean_stdwght, sd_stdwght, min_stdwght, max_stdwght),
    decimals = 1
  )


