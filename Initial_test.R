### R Attempt #3
#####
library(tidyverse)
library(lubridate)

# Define the function
parse_target_dat <- function(filepath) {
  lines <- read_lines(filepath) %>%
    str_trim() %>%
    discard(~ .x == "")
  
  lake_code <- lines[str_detect(lines, "lake code")] %>%
    str_extract("^\\S+")
  lake_code <- rep(lake_code, length.out = 1737)  # Repeat for all rows
  
  date_str <- lines[str_detect(lines, ": date")] %>%
    str_extract("^\\d{6}")
  survey_date <- ymd(date_str[1])  # Ensure it's a single date
  survey_date <- rep(survey_date, length.out = 1737)  # Repeat for all rows
  
  sounder <- lines[str_detect(lines, "FURUNO|SIMRAD")] %>%
    str_extract("FURUNO|SIMRAD")
  sounder <- rep(sounder, length.out = 1737)  # Repeat for all rows
  
  gain <- lines[str_detect(lines, "Gain")] %>%
    str_extract("^\\d+") %>%
    as.numeric()
  gain <- rep(gain, length.out = 1737)  # Repeat for all rows
  
  # Find the header index
  data_header_index <- which(str_detect(lines, "depth / transect / targets"))
  print(data_header_index)  # Check where the header is located
  
  # Helps us extract data starting from the correct position
  if(length(data_header_index) > 1) {
    warning("Multiple header occurrences detected. Using the first occurrence.")
  }
  
  # Extract data starting from the header
  data_lines <- lines[(data_header_index[1] + 1):length(lines)]
  print(head(data_lines, 10))  # Inspect the first 10 lines of raw data
  
  # Manually split data into columns
  parsed_data <- data_lines %>%
    str_split("\\s+") %>%  # Split by any whitespace
    map(~ .x[1:5]) %>%  # Take only the first 5 columns
    map_dfr(~ tibble(
      # Keep depth as a range without averaging
      depth = .x[1] %>%
        str_extract("^\\d+[-]\\d+$"),
      
      transect = as.numeric(.x[2]),
      targets = as.numeric(.x[3]),
      pct_sockeye = as.numeric(.x[4]),
      pct_stickleback = as.numeric(.x[5])
    ))
  
  # Check parsed data for NAs or other issues
  print(head(parsed_data))  # Inspect the first few rows
  
  # Check for rows where 'depth' is NA
  na_depth_rows <- parsed_data %>%
    filter(is.na(depth))
  print(na_depth_rows)  # Print rows where depth is NA
  
  # Add new columns (repeat values for all rows)
  parsed_data <- parsed_data %>%
    mutate(
      lake_code = lake_code,
      survey_date = survey_date,
      sounder_type = sounder,
      gain = gain
    )
  
  return(parsed_data)
}

file_path <- "TARGET77.DAT"
target_data <- parse_target_dat(file_path)

### ### printing and saving to a csv
print(target_data)
write.csv(target_data, "TARGET77.csv", row.names = FALSE)
