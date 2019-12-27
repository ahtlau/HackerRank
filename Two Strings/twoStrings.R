# Author: Alex Liu
# Last modified: 2019-12-27
# Description: This scripts tests if there are any common letters in any two given strings.

# Setup -------------------------------------------------------------------

remove(list = ls())

library(stringr)
library(dplyr)

dir <- "/Users/ahtlau/Library/Mobile Documents/com~apple~CloudDocs/Data/HackerRank/Two Strings"

setwd(dir)

# Data --------------------------------------------------------------------

# Load data
(raw <- readLines(file.path("input", "input00.txt"), warn = F))
# (raw <- readLines(file.path("input", "input06.txt"), warn = F))
# (raw <- readLines(file.path("input", "input07.txt"), warn = F))

# Parse the first number, which is the number of strings for comparisons in each test case
(n_cases <-  raw[1] %>% str_trim() %>% as.integer())

# Get all the (raw) strings 
(all_raw_strings <- raw[-1])

# Count the total number of strings 
(total_n_strings <- length(all_raw_strings))

# Count the number of strings in each test case
(n_strings_to_compare <- total_n_strings / n_cases)

# This function splits the a string into letters and removes duplicates
clean_string <- function (i) {
  (s <- all_raw_strings[i] %>% 
     str_split("") %>% 
     unlist() %>% 
     unique()) 
}

# Run raw strings through this function to get cleaned strings
(all_cleaned_strings <- lapply(1:length(all_raw_strings), clean_string))

# This function tests if there are common letters in both strings
# If there are, the table will return a value greater than 1
test <- function (j) {
  # On HackerRank there are always two strings for comparison
  strings_filtered <- all_cleaned_strings[j:(j+1)] 
  # Collapse filtered strings, count the number of occurences for each letter
  count <- table(strings_filtered %>% 
                   unlist())
  if (any(count > 1) == TRUE) {
    return("YES")
  } else {
    return("NO")
  }
}

# Create a vector of indices of the first string in each test group
(indices_of_1st_string <- seq(1, total_n_strings, by = n_strings_to_compare))

# Call this function for every test case
test_results <- lapply(indices_of_1st_string, test)

# Print the results without quotation marks
print(noquote(test_results %>% unlist()))

# The end

