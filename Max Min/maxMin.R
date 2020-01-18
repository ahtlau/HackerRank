# Author: Alex Liu
# Last modified: 2020-01-18
# Description: This script determines the minimum unfairness of a subset of an arrray

# Setup -------------------------------------------------------------------

remove(list = ls())

library(stringr)
library(dplyr)

dir <- "/Users/ahtlau/Library/Mobile Documents/com~apple~CloudDocs/Data/HackerRank/Max Min"
setwd(dir)

# Analysis ----------------------------------------------------------------

# Load data
(raw <- readLines(file.path("input", "input00.txt"), warn = F))
# (raw <- readLines(file.path("input", "input01.txt"), warn = F))
# (raw <- readLines(file.path("input", "input15.txt"), warn = F))
# (raw <- readLines(file.path("input", "input16.txt"), warn = F))

# Extract k
(k <- raw[2] %>% as.double())

# Extract array
(arr <- raw[-c(1:2)] %>% as.double())

# Sort array in an ascending order
(sorted_arr <- sort(arr))

# Define a function to find local min for an possible combos of sub arrays
find_unfairness <- function (i) {
  (min <- sorted_arr[i])
  (max <- sorted_arr[i + (k - 1)])
  (unfairness = max - min)
}

# Save result and find global min
(result <- lapply(1:(length(sorted_arr) - k + 1), find_unfairness) %>% 
    unlist() %>% 
    min())



