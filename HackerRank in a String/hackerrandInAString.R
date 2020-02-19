# Author: Alex Liu
# Last modified: 2020-02-19
# Description: This script detects if a string contains the word "hackerrank" 

# Setup -------------------------------------------------------------------

remove(list = ls())

library(stringr)
library(dplyr)

dir <- "/Users/ahtlau/Library/Mobile Documents/com~apple~CloudDocs/Data/HackerRank/HackerRank in a String"
setwd(dir)

# Analysis ----------------------------------------------------------------

# Load data
(raw <- readLines(file.path("input", "input00.txt"), warn = F))
# (raw <- readLines(file.path("input", "input03.txt"), warn = F))

# Extract the number of test cases
(n_cases <- raw[1] %>% as.double())

# Extract all test case materials 
(cases <- raw[-1])

# Create hackerrank pattern
(pattern <- str_split("hackerrank", "") %>% unlist())

test_string <- function (i) {
  # Split a string into individual letters
  (letters <- str_split(cases[i], "") %>% unlist())
  
  # Assign initital index 
  j <- 1
  k <- 1
  
  repeat {
    # Detect if one of the letters is in the string
    (temp <- str_detect(letters, pattern[k]))
    if (any(temp) == TRUE) {
      # If true, find where this letter is located
      (index <- match(pattern[k], letters))
      # Trim string up to where the detected letter ends. 
      # Remaining string is saved for further analysis. 
      (letters <- letters[-c(1:index)])
      j <- j + 1
      k <- k + 1
    } else {
      # If there is no TRUE, break
      result <- "NO"
      break
    }
    # Break is all letters in patterns have been tested
    if (k > length(pattern)) {
      result <- "YES"
      break
    }
  }
  return(result)
}

# Run function through all test cases
result <- lapply(1:n_cases, test_string) %>% unlist()

# Print results vertically
cat(paste(result, collapse = "\n"))


