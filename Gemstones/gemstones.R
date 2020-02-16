# Author: Alex Liu
# Last modified: 2020-02-15
# Description: This script all the common letters across all given strings

# Setup -------------------------------------------------------------------

remove(list = ls())

library(stringr)
library(dplyr)

dir <- "/Users/ahtlau/Library/Mobile Documents/com~apple~CloudDocs/Data/HackerRank/Gemstones"
setwd(dir)

# Analysis ----------------------------------------------------------------

# Load data
(raw <- readLines(file.path("input", "input27.txt"), warn = F))
# (raw <- readLines(file.path("input", "input28.txt"), warn = F))
# (raw <- readLines(file.path("input", "input29.txt"), warn = F))
# (raw <- readLines(file.path("input", "input30.txt"), warn = F))

# Extract all strings 
(arr <- raw[-1])

# Break down a string into individual unique letters
(arr_clean <- lapply(1:length(arr), function (i) {str_split(arr[i], "") %>% unlist() %>% unique()}))

# Find common letters within all strings
(n <- length(Reduce(intersect, arr_clean)))




