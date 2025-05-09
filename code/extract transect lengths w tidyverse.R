#extract transect lengths w tidyverse.R
  # asked chatGTP to convert "extract transect lengths May1-2.R"
  # to tidyverse
  
# good start, but definitely problems with output 
# (e.g., stratum 2-5 is treated as a date!; don't need to paste "Transect" in front of transect number, etc) 

library(tidyverse)
library(readr) # for write_csv()

# setwd("C:/Rcode/ATS")

file_path <- "./data/transect_lengths.txt" # "TARGET.txt"

# Read entire file
target_dat <- readLines(file_path)

# Find all lines where "Strat." appears (+2 for table start)
strat_lines <- grep("Strat\\.", target_dat) + 2  

lake_info <- map_df(seq_along(strat_lines), function(i) {
  
  lake_line <- strat_lines[i] - 7  # Identify lake name position
  
  # Extract lake name and code
  lk_name <- str_extract(target_dat[lake_line], "^[^ ]+")
  lk_code <- str_extract(target_dat[lake_line], "\\d+")
  
  # Extract number of transects
  num_transects <- as.numeric(str_extract(target_dat[lake_line + 2], "\\d+"))
  
  # Extract data lines
  temp_vec <- target_dat[strat_lines[i]:(strat_lines[i] + num_transects - 1)]
  
  # Convert into tibble
  temp_df <- read.table(text = temp_vec, header = FALSE) %>%
    set_names(c("Stratum", "Area", paste0("Transect", seq_len(ncol(.) - 2)))) %>%
    pivot_longer(cols = -c(Stratum, Area), names_to = "Transect", values_to = "Length") %>%
    mutate(Lake = lk_name, LakeCode = lk_code)  # Add lake metadata
 
  return(temp_df)
})

# Final structured dataset
print(lake_info)  

write_csv(lake_info, "./data/lake_transect_stratum_lengths.csv")
  
