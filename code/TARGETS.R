## --------------------------------------------------------------------------
## TARGETS.R
##
## Title:   Import echosounder target count data  
## Purpose: 
## Author:  H Stiff
## Date:    20.
## Notes:   
##          
## --------------------------------------------------------------------------

# Function to read metadata
read_metadata <- function(data, start_line) {
  metadata <- list()
  metadata$lake_code <- substr(data[start_line], 1, 10)
  metadata$lake_code_label <- sub(".*: ", "", data[start_line])
  metadata$text_note <- data[start_line + 1]
  metadata$survey_date <- substr(data[start_line + 2], 1, 6)
  metadata$survey_date_label <- sub(".*: ", "", data[start_line + 2])
  metadata$sounder <- substr(data[start_line + 3], 21, 22)
  metadata$sounder_label <- sub(".*: ", "", data[start_line + 3])
  metadata$sounder_gain <- substr(data[start_line + 4], 1, 3)
  metadata$text_comment <- data[start_line + 5]
  return(metadata)
}

# Function to read survey data
read_survey_data <- function(data, start_line) {
  survey_data <- data.frame(
    depth = character(),
    transect = integer(),
    targets = integer(),
    percent_sockeye = numeric(),
    percent_stickle = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Skip two blank lines and the header line
  line <- start_line + 3
  
  while (line <= length(data) && data[line] != "") {
    depth <- substr(data[line], 1, 5)
    transect <- as.integer(substr(data[line], 8, 9))
    targets <- as.integer(substr(data[line], 12, 16))
    percent_sockeye <- as.numeric(substr(data[line], 20, 23))
    percent_stickle <- as.numeric(substr(data[line], 26, 29))
    
    survey_data <- rbind(survey_data, data.frame(
      depth = depth,
      transect = transect,
      targets = targets,
      percent_sockeye = percent_sockeye,
      percent_stickle = percent_stickle,
      stringsAsFactors = FALSE
    ))
    
    line <- line + 1
  }
  
  return(list(survey_data = survey_data, next_line = line + 1))
}

# Main function to read the entire file
read_target77 <- function(file_path) {
  data <- readLines(file_path)
  surveys <- list()
  line <- 1
  
  while (line <= length(data)) {
    if (data[line] != "") {
      metadata <- read_metadata(data, line)
      survey_data_result <- read_survey_data(data, line + 6)
      metadata$survey_data <- survey_data_result$survey_data
      surveys <- append(surveys, list(metadata))
      line <- survey_data_result$next_line
    } else {
      line <- line + 1
    }
  }
  
  return(surveys)
}

# Read the data from the TARGET file
file_path <- "./3_Data/TARGET77.DAT"
file_path <- "C:/DFO-MPO/OneDrive/OneDrive - DFO-MPO/SIRE-P - Living Data Project - Yuliya/3_Data/1_In_Process/TARGET77.DAT"
surveys <- read_target77(file_path)


# Print the extracted information
for (survey in surveys) {
  cat("Lake Code:", survey$lake_code, "\n")
  cat("Lake Code Label:", survey$lake_code_label, "\n")
  cat("Text Note:", survey$text_note, "\n")
  cat("Survey Date:", survey$survey_date, "\n")
  cat("Survey Date Label:", survey$survey_date_label, "\n")
  cat("Sounder:", survey$sounder, "\n")
  cat("Sounder Label:", survey$sounder_label, "\n")
  cat("Sounder Gain:", survey$sounder_gain, "\n")
  cat("Text Comment:", survey$text_comment, "\n")
  cat("Survey Data:\n")
  print(survey$survey_data)
  cat("\n")
}
