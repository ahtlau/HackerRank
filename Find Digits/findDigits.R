# Author: Alex Liu
# Last modified: 2019-12-30
# Description: This script finds the number of digits - split from any given integer - that can evenly 
# divide the integer

# Setup -------------------------------------------------------------------

remove(list = ls())

dir <- "/Users/ahtlau/Library/Mobile Documents/com~apple~CloudDocs/Data/HackerRank/Find Digits"
setwd(dir)

library(stringr)
library(dplyr)

# Analysis ----------------------------------------------------------------

# Load data
(raw <- readLines(file.path("input", "input00.txt"), warn = F))

# Extract and clean arrays
(arrays <- raw[-1] %>% 
    str_split(" ") %>% 
    unlist() %>% 
    str_trim() %>% 
    as.double())

# Write a function to make a data frame
find_digits <- function (i) {
  arrays[i] %>% 
    str_split("") %>% 
    unlist() %>% 
    str_trim() %>% 
    as.double() %>% 
    as.data.frame() %>% 
    rename("digits" = ".") %>% 
    mutate(number = arrays[i]) %>% 
    select(number, digits) %>% 
    mutate(remainder = number %% digits) %>% 
    filter(remainder == 0) %>% 
    group_by(number) %>% 
    count(number) %>% 
    ungroup() %>% 
    pull(n)
}

# Call the function
(results <- lapply(1:length(arrays), find_digits) %>% unlist())

# Print results vertically
cat(results, sep = "\n")
  


