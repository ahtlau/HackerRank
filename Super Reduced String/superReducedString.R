# Author: Alex Liu
# Last modified: 2020-02-15
# Description: This script determines the final string which does not contain duplicate letters

# Setup -------------------------------------------------------------------

remove(list = ls())

library(stringr)
library(dplyr)

dir <- "/Users/ahtlau/Library/Mobile Documents/com~apple~CloudDocs/Data/HackerRank/Super Reduced String"
setwd(dir)

# Analysis ----------------------------------------------------------------

# Load data
(raw <- readLines(file.path("input", "input00.txt"), warn = F))
# (raw <- readLines(file.path("input", "input01.txt"), warn = F))
# (raw <- readLines(file.path("input", "input03.txt"), warn = F))

# Split string into unique individual letters
(split_letters <- str_split(raw, "") %>% unlist() %>% unique())

# Save raw into anther object for analysis 
(result <- raw)

# Assign initital index
i = 1

# Remvoe letters
repeat {
  # For each letter, remove if there are exactly 2 consecutive letters
  (pattern <- paste0(split_letters[i], "{2}"))
  (result <- str_remove(result, pattern))
  i <- i + 1
  # Break if the index exceeds the length of the letter
  if (i > length(split_letters)) {
    break
  }
}

if (any(str_detect(result, "[a-z]")) == FALSE) {
    result <- "Empty String"
} 

noquote(result)


