# Author: Alex Liu
# Last modified: 2020-02-15
# Description: This script determines the number of subarrays that satisfy the beautiful triplets criteria

# Setup -------------------------------------------------------------------

remove(list = ls())

library(stringr)
library(dplyr)
library(reshape2)

dir <- "/Users/ahtlau/Library/Mobile Documents/com~apple~CloudDocs/Data/HackerRank/Beautiful Triplets"
setwd(dir)

# Analysis ----------------------------------------------------------------

# Load data
(raw <- readLines(file.path("input", "input00.txt"), warn = F))
# (raw <- readLines(file.path("input", "input10.txt"), warn = F))

# Write cleaning function
clean <- function (i) {
  temp <- raw[i] %>% 
    str_split(., " ") %>% 
    unlist() %>% 
    str_trim(side = "both") %>% 
    as.double()
}

# Extract d value
(d <- clean(i = 1)[2])

# Clean array
(arr <- clean(i = 2))

# Count the number of occurances for each elements
(count <- table(arr) %>% melt())

# Turn array into a data frame
(n <- data.frame(array = arr, stringsAsFactors = F) %>%
    # Arrange elements in a descening order
    arrange(desc(array)) %>%
    # Calculate the differences 
    mutate(diff = array - d,
           diff_2 = diff - d) %>%
    # If numbers in diff and diff_2 are in the original array, flag true and filter on them
    mutate(test = if_else(diff %in% array & diff_2 %in% array, T, F)) %>%
    filter(test == T) %>% 
    select(-test) %>% 
    # For each case, create a unique id
    mutate(id = row_number()) %>%
    # Reshape to long
    melt(id.vars = c("id")) %>% 
    # Join occurances data
    left_join(count, by = c("value" = "arr")) %>% 
    # For each group, calcualte all of the combinations and then sum across each case
    group_by(id) %>% 
    summarise(value = prod(value.y)) %>% 
    ungroup() %>% 
    pull(value) %>% 
    sum())

