# extract transect lengths May1-2.R
  #clean up code

library(tidyverse)
library(readr) # for write_csv()

# setwd("C:/Rcode/ATS")
#"TARGET.txt" is "TARGET.TRN", but just with the extension changed.
# file.path <- "TARGET.txt"

file_path <- "./data/transect_lengths.txt" # was "TARGET.txt"

library(tidyverse)
library(tidyr)  # for wide to long format
library(stringr)  # for counting words in a string

# Read entire TARGET.txt file as raw text
target.dat <- readLines(file_path)

# #find all lines that have "Strat", +2; these are the lines where the data tables begin
strat.lines <- grep("Strat\\.", target.dat) + 2  

lake <-vector()
num.transects <- vector()
temp.vec <- vector()
temp.df <- data.frame()
length.df <- data.frame()

# loop to extract transect lengths
for (i in 1: length(strat.lines)){

  # lake[i] starts on this line
  lake[i] <- strat.lines[i]-7  # [1] 1 for i=1

  # get lake name as everything before the first space in the string
  lk.name <- sub(" .*", "", target.dat[lake[i]])    

  # Extract the lake code as a character string
  lk.code <- sub(".*?(\\d+)\\s:.*", "\\1", target.dat[lake[i]])    

  # get number of strata, to use to find end line of table;
    # str_extract(text, "\\d+")  -  finds the first numeric sequence in the string
  num.transects <- as.numeric(str_extract(target.dat[lake[i]+2], "\\d+"))

  # data table starts on line: strat.lines[i] 

  # make a temporary character vector with the data from only one table
  temp.vec <- target.dat[strat.lines[i]: (strat.lines[i] + num.transects - 1)] 

  # Convert to a dataframe while handling whitespace
  temp.df <- read.table(text = temp.vec, header = FALSE)
  
  # Rename the first two columns
  colnames(temp.df)[1:2] <- c("Stratum", "Area")
  
  # Rename the remaining columns sequentially
  colnames(temp.df)[3:ncol(temp.df)] <- seq_len(ncol(temp.df) - 2)
 
  # Convert to long format; this is a tibble
  df.long <- temp.df %>%
    pivot_longer(cols = -c(Stratum, Area), names_to = "Transect", values_to = "Length")

  # turn back into a df
  df.long <- as.data.frame(df.long)
    
  df.long <- cbind(lk.name, lk.code, df.long)   

  length.df <-  rbind(length.df, df.long) 
} 

# try to force Excel to read Stratum as text strings--didn't work
length.df$Stratum <- as.character(length.df$Stratum)

write.csv(length.df, "./data/LakeStrataLengths.csv", row.names = FALSE)

