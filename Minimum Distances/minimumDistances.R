# Author: Alex Liu
# Last modified: 2020-01-20
# Description: This script determines the minimum distance between two indices of two matching numbers

# Setup -------------------------------------------------------------------

remove(list = ls())

library(stringr)
library(dplyr)

dir <- "/Users/ahtlau/Library/Mobile Documents/com~apple~CloudDocs/Data/HackerRank/Minimum Distances"
setwd(dir)

# Analysis ----------------------------------------------------------------

# Load data
(raw <- readLines(file.path("input", "input00.txt"), warn = F))
# (raw <- readLines(file.path("input", "input01.txt"), warn = F))
# (raw <- readLines(file.path("input", "input02.txt"), warn = F))

# Clean data
(arr <- raw[2] %>% 
    str_trim(side = "both") %>% 
    str_split(., " ") %>% 
    unlist() %>% 
    as.double() %>% 
    as.data.frame() %>% 
    rename("arr" = ".") %>% 
    mutate(index = row_number()))

# Find matching number pairs
(matching_arr <- arr %>% 
    count(arr) %>% 
    filter(n > 1) %>% 
    select(arr))

# If else condition
if (nrow(matching_arr) >= 1) {
  
  min <- arr %>% 
    semi_join(matching_arr, by = c("arr")) %>% 
    arrange(arr) %>% 
    group_by(arr) %>% 
    mutate(lag_index = lag(index),
           diff = abs(index - lag_index)) %>% 
    filter(!is.na(diff)) %>% 
    pull(diff) %>% 
    min()
  
} else {
  min <- -1
}

min

