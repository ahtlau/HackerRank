# Author: Alex Liu
# Last modified: 2020-02-08
# Description: This script finds the median of a given array

# Setup -------------------------------------------------------------------

remove(list = ls())

library(stringr)
library(dplyr)

dir <- "/Users/ahtlau/Library/Mobile Documents/com~apple~CloudDocs/Data/HackerRank/Find the median"
setwd(dir)

# Analysis ----------------------------------------------------------------

# Load data
(raw <- readLines(file.path("input", "input00.txt"), warn = F))

# Clean array
(arr <- raw[2] %>% 
  str_split(., " ") %>% 
  unlist() %>% 
  str_trim(side = "both") %>% 
  as.double())

# Find median
(median <- median(sort(arr)))
