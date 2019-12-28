# Author: Alex Liu
# Last modified: 2019-12-27
# Description: This script finds the minimum number of deletions such that there are no matching adjacent letters
# in any given string

# Setup -------------------------------------------------------------------

remove(list = ls())

library(stringr)
library(dplyr)

dir <- "/Users/ahtlau/Library/Mobile Documents/com~apple~CloudDocs/Data/HackerRank/Alternating Characters"
setwd(dir)

# Analysis ----------------------------------------------------------------

# Load data
(raw <- readLines(file.path("input", "input00.txt"), warn = F))
# (raw <- readLines(file.path("input", "input13.txt"), warn = F))
# (raw <- readLines(file.path("input", "input14.txt"), warn = F))

# Find all of the strings for testing
(all_cases <- raw[-1])

test_deletions <- function (i) {
  
  # Split string into individual letters
  (string_original <- all_cases[i] %>% 
     str_split("") %>% 
     unlist())
  
  # Create a vector containing the neighboring letters
  (string_adjusted <- string_original[-1])
  (string_adjusted[length(string_original)] <- "NA")
  
  # Create a dataframe with these two vectors
  (df <- data.frame(letter = string_original,
                    next_letter = string_adjusted))
  
  # If a letter is the same as adjacent letter, they have the same value in a rowwise comparison
  # Flag TRUE if both are the same. FALSE otherwise.
  # Each TRUE is one deletions
  # Sum up all TRUE's to get total number of deletions
  (df_test <- df %>% 
      mutate(letter = as.character(letter),
             next_letter = as.character(next_letter),
             flag = if_else(letter == next_letter, TRUE, FALSE)) %>% 
      pull(flag) %>% 
      sum() %>% 
      as.integer())
}

(results <- lapply(1:length(all_cases), test_deletions) %>% 
  unlist())

# Print results vertically
cat(results, sep = "\n", fill = TRUE)


