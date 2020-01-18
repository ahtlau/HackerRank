# Author: Alex Liu
# Last modified: 2020-01-18
# Description: This script determines the number of paired numbers whose difference equals the target value

# Setup -------------------------------------------------------------------

remove(list = ls())

library(stringr)
library(dplyr)

dir <- "/Users/ahtlau/Library/Mobile Documents/com~apple~CloudDocs/Data/HackerRank/Pairs"
setwd(dir)

# Analysis ----------------------------------------------------------------

# Load data
(raw <- readLines(file.path("input", "input15.txt"), warn = F))
# (raw <- readLines(file.path("input", "input16.txt"), warn = F))
# (raw <- readLines(file.path("input", "input17.txt"), warn = F))
# raw <- readLines(file.path("input", "input18.txt"), warn = F)
# raw <- readLines(file.path("input", "input19.txt"), warn = F)
# raw <- readLines(file.path("input", "input20.txt"), warn = F)
# raw <- readLines(file.path("input", "input21.txt"), warn = F)

# Clean string function
clean <- function (i) {
  raw[i] %>% 
    str_split(., " ") %>% 
    unlist() %>% 
    str_trim(side = "both") %>% 
    as.double()
}

# Extract number of elements, targat value, and array
(n_elements <- clean(i = 1)[1])
(target <- clean(i = 1)[2])
(arr <- clean(i = 2))

# # Subtract target from the array
(arr_diff <- arr - target)

# filter on values that are in the difference array and count the 
(n_pairs <- data.frame(arr) %>% 
    filter (arr %in% arr_diff) %>% 
    nrow())

# If NA, assign 0
if (is.na(n_pairs)) {
  n_pairs <- 0
  }



