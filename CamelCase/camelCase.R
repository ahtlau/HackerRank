# Author: Alex Liu
# Last modified: 2020-02-08
# Description: This script finds the number of words in contained in a given string

# Setup -------------------------------------------------------------------

remove(list = ls())

library(stringr)
library(dplyr)

dir <- "/Users/ahtlau/Library/Mobile Documents/com~apple~CloudDocs/Data/HackerRank/CamelCase"
setwd(dir)

# Analysis ----------------------------------------------------------------

# Load data
(raw <- readLines(file.path("input", "input00.txt"), warn = F))
# (raw <- readLines(file.path("input", "input01.txt"), warn = F))

# Break the string into individual letters
(arr <- str_split(raw, "") %>% unlist())

# tabulate counts for TRUE and FALSE
(summary <- table(str_detect(arr, "[[:upper:]]")))

# If NA (i.e. no upper case letters), assign a value of 1 (i.e., a single word)
if (is.na(as.double(summary[2]))) {
  (n <- 1)
  # If no NA, add one to count for the lower case word at the beginning of string
} else {
  (n <- as.double(summary[2]) + 1)
}
