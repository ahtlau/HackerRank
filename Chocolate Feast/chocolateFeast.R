# Author: Alex Liu
# Last modified: 2020-02-08
# Description: This script determines the number of chocalate that can be bought and exchanged

# Setup -------------------------------------------------------------------

remove(list = ls())

library(stringr)
library(dplyr)

dir <- "/Users/ahtlau/Library/Mobile Documents/com~apple~CloudDocs/Data/HackerRank/Chocolate Feast"
setwd(dir)

# Analysis ----------------------------------------------------------------

(raw <- readLines(file.path("input", "input00.txt"), warn = F))
# (raw <- readLines(file.path("input", "input10.txt"), warn = F))
# (raw <- readLines(file.path("input", "input11.txt"), warn = F))

# Extract number of test cases
(n_cases <- as.double(raw[1]))

# Extract all the case elements
(cases <- raw[-1])

gen_results <- function (i) {
  
  # Clean array
  (cases_clean <- cases[i] %>%  
     str_split(., " ") %>% 
     unlist() %>% 
     str_trim(side = "both") %>% 
     as.double())
  
  # Extract budget, price and exchange threshold
  (budget <- cases_clean[1])
  (price <- cases_clean[2])
  (threshold <- cases_clean[3])
  
  # Number of chococate one can buy with budget
  (n_choc = floor(budget/price))
  
  # If # of wrapper is exactly the number of exchange threshold, 
  # the person gets 1 additional chocolate
  if (n_choc == threshold) {
    n_choc <- n_choc + 1 
    # If # of wrapper is less than the number of exchange threshold, 
    # the person does not get any
  } else if (n_choc < threshold) {
    n_choc <- n_choc + 0
    # Case when # of wrapper is greater than threshold
  } else {
    n_wrapper <- n_choc
    repeat {
      # Calculate left_over # of wrappers after exchange 
      remaining_wrapper <- n_wrapper %% threshold
      # Calculate prevailing # of wrappers after excahnge
      # This is simply the number of chocolate that has been exchanged 
      n_wrapper <- floor(n_wrapper/threshold)
      # Calculate prevailing chocolate that have been eaten
      n_choc <- n_choc + n_wrapper
      # Break if the sum of left_over wrappers is less than threshold
      if (n_wrapper + remaining_wrapper < threshold) {
        break
        # Continue if sum is greater than threshold
      } else {
        n_wrapper <- n_wrapper + remaining_wrapper
      }
    }
  }
  return(n_choc)
}

# Run function through each test case
(results <- lapply(1:n_cases, gen_results) %>% unlist())

# Print results vertically
(vertical <- cat(paste0(results, collapse = '\n')))


