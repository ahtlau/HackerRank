# Author: Alex Liu
# Last modified: 2020-02-09
# Description: This script determines the the min # of deletions required to make remaining elements all equal

# Setup -------------------------------------------------------------------

remove(list = ls())

library(stringr)
library(dplyr)

dir <- "/Users/ahtlau/Library/Mobile Documents/com~apple~CloudDocs/Data/HackerRank/Equalize the Array"
setwd(dir)

# Analysis ----------------------------------------------------------------

(raw <- readLines(file.path("input", "input00.txt"), warn = F))
# (raw <- readLines(file.path("input", "input17.txt"), warn = F))

# Extract the number of elements 
(n_elements <- raw[1] %>% as.double())

# Clean number array
(arr <- raw[2] %>% 
  str_split(., " ") %>% 
  unlist() %>% 
  str_trim(side = "both") %>% 
  as.double())

# Tabulate count for each unique element, extract the max
(n_max <- max((table(arr))))

# # of deletions is # of elements less max
(n_deletion <- n_elements - n_max)
