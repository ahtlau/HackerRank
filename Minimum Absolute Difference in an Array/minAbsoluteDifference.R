# Author: Alex Liu
# Last modified: 2019-12-27
# Description: This scripts finds the min difference among values in a given array

# Setup -------------------------------------------------------------------

remove(list = ls())

options(scipen = 999)

dir <- "/Users/ahtlau/Library/Mobile Documents/com~apple~CloudDocs/Data/HackerRank/Minimum Absolute Difference in an Array"
setwd(dir)

library(stringr)
library(dplyr)

# Analysis ----------------------------------------------------------------

# Load data
# raw <- readLines(file.path("input", "input00.txt"), warn = F)
# raw <- readLines(file.path("input", "input01.txt"), warn = F)
# raw <- readLines(file.path("input", "input10.txt"), warn = F)
# raw <- readLines(file.path("input", "input12.txt"), warn = F)
raw <- readLines(file.path("input", "input13.txt"), warn = F)

# Number of elements
n_elements <- raw[1] %>% 
    as.double()

# Extract number arrays, sort in ascending order
array <- raw[2] %>% 
    str_split(" ") %>% 
    unlist %>% 
    as.double() %>% 
    sort()

# Test if there are any identical elements
# If so, the absolute difference is 0
# If not, find absolute difference among neighboring values
# If values are close to each other, their difference should be smaller
if (any(table(array) > 1) == TRUE) {
  results <- 0
} else {
  # Delete first element
  array_adjusted <- array[-1]
  # Assign the last element to be NA to make sure the length is the same as the original vector
  # Having the same length is neccessary to create a data frame
  array_adjusted[n_elements] <- NA
  # Create a data frame with these vectors. This would allow for side-by-side comparison. 
  df <- data.frame(original_value = array,
                    adjusted_value = array_adjusted)
  # Take the difference between the two columns. Find the min difference. 
  results <- df %>% 
      mutate(diff = abs(original_value - adjusted_value)) %>% 
      filter(!is.na(diff)) %>% 
      pull() %>% 
      min()
}

results


