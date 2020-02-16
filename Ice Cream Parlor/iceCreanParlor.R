# Author: Alex Liu
# Last modified: 2020-02-16
# Description: This script finds the a pair of prices whose sum equals available budget

# Setup -------------------------------------------------------------------

remove(list = ls())

library(stringr)
library(dplyr)

dir <- "/Users/ahtlau/Library/Mobile Documents/com~apple~CloudDocs/Data/HackerRank/Ice Cream Parlor"
setwd(dir)

# Analysis ----------------------------------------------------------------

# Load data
(raw <- readLines(file.path("input", "input00.txt"), warn = F))
# (raw <- readLines(file.path("input", "input03.txt"), warn = F))
# raw <- readLines(file.path("input", "input04.txt"), warn = F)

# Determine the numbe of cases
(n_cases <- raw[1] %>% as.double())

# Find all the cases
(cases <- raw[-1])

find_ice_cream <- function (i) {
  
  # Extract all the elements in a case
  (case <- cases[i:(i+2)])
  
  # Find budget
  (budget <- case[1] %>% as.double())
  
  # Find costs of all ice cream
  (costs <- str_split(case[-c(1:2)], " ") %>% unlist() %>% as.double())
  
  # # Filter out costs that are greater or equal to the budget
  (available_costs <- costs[costs < budget])
  
  j <- 1
  
  repeat {
    # Start with j element
    (test <- available_costs[j])
    # Calculate the complement of that i element
    (pair <- budget - test)
    # Test if they are in the available costs array. If not move on to j + 1 case
    if (any(available_costs[-j] == pair) == TRUE) {
      # Use "which" command if they are the same
      if (any(test == pair) == TRUE) {
        (result <- paste0(which(pair == costs), collapse = " "))
        return(result) 
        break
      } else {
        # Use "match" command if they are the different
        (result <- paste0(match(c(test, pair), costs), collapse = " "))
        return(result)
        break
      } 
    } else {
      j <- j + 1
    }
  }
}

# Create a sequence of indices where the first elemment of a case begins
sequence <- seq(from = 1, to = length(cases), by = 3)

# Apply function to all cases
(results <- lapply(sequence, find_ice_cream) %>% unlist())

# Print results vertically
cat(paste(noquote(results), collapse = "\n"))

# (raw_2 <- readLines(file.path("output", "output04.txt"), warn = F))
#  
# table(results == raw_2)



