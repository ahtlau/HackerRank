# Author: Alex Liu
# Last modified: 2020-02-09
# Description: This script determines the number of square integers in a range between two given integers

# Setup -------------------------------------------------------------------

remove(list = ls())

library(stringr)
library(dplyr)

dir <- "/Users/ahtlau/Library/Mobile Documents/com~apple~CloudDocs/Data/HackerRank/Sherlock and squares"
setwd(dir)

# Analysis ----------------------------------------------------------------

# Load data
(raw <- readLines(file.path("input", "input08.txt"), warn = F))
# (raw <- readLines(file.path("input", "input09.txt"), warn = F))
# (raw <- readLines(file.path("input", "input10.txt"), warn = F))

# Determine the number of test cases
(n_cases <- raw[1] %>% as.double())

# Find a vector which contains all test cases
(cases <- raw[-1])

gen_results <- function (i) {
  # Clean character array into number array
  (arr <- cases[i] %>% 
     str_split(., " ") %>% 
     unlist() %>% 
     str_trim(side = "both") %>% 
     as.double())
  
  # Extract lower and upper bounds
  (lower <- arr[1]) 
  (upper <- arr[2])
  
  # Determine the square root lower and upper bounds
  (sqrt_lower <- ceiling(sqrt(lower)))
  (sqrt_upper <- floor(sqrt(upper)))
  
  # The number of square integer equals the difference between the two plus one
  (n <- sqrt_upper - sqrt_lower + 1)
}

(result <- lapply(1:n_cases, gen_results) %>% unlist())
