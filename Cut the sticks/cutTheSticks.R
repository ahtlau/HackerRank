# Author: Alex Liu
# Last modified: 2020-01-12
# Description: This script determines the length of an array after successive subtraction of its min

# Setup -------------------------------------------------------------------

remove(list = ls())

dir <- "/Users/ahtlau/Library/Mobile Documents/com~apple~CloudDocs/Data/HackerRank/Cut the sticks"
setwd(dir)

library(dplyr)
library(stringr)

# Analysis ----------------------------------------------------------------

# Load data
(raw <- readLines(file.path("input", "input08.txt"), warn = F))
# (raw <- readLines(file.path("input", "input09.txt"), warn = F))

# Extract and clean array
clean_string <- function (i) {
  (raw[i] %>% 
     str_split(" ") %>% 
     unlist() %>% 
     str_trim(side = "both") %>% 
     as.double())
}

# Call function to each string
(length_numbers <- clean_string(i = 1))
(numbers <- clean_string(i = 2))

# Create a vector where results will be saved
# Assign the first result straight out of the input
results <- c()
i <- 1
(results[i] <- length_numbers)

# Saved numbers in another object which will be used for analysis
(numbers_trimmed <- numbers)

while (length(numbers_trimmed) > 0) {
  # Save min value
  (min <- min(numbers_trimmed))
  # Subtract min from the numbers 
  (numbers_trimmed <- numbers_trimmed - min)
  # Filter out any 0 numbers
  (numbers_trimmed <- numbers_trimmed[numbers_trimmed != 0])
  # Take a measure of the length and save the results
  (results[i + 1] <- length(numbers_trimmed))
  i <- i + 1
}

# Print results without the last entry
(results <- results[results != 0])

# print results vertically
(results_vertical <- cat(results, sep = "\n"))


