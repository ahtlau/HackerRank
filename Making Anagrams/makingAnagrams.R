# Author: Alex Liu
# Last modified: 2019-12-27
# Description: This scripts finds the minimum deletions required to make any given two strings 
# to be anagrams of each other

# Setup -------------------------------------------------------------------

remove(list = ls())

dir <- "/Users/ahtlau/Library/Mobile Documents/com~apple~CloudDocs/Data/HackerRank/Making Anagrams"
setwd(dir)

library(dplyr)
library(stringr)

# Analysis ----------------------------------------------------------------

# Load data
(raw <- readLines(file.path("input", "input00.txt"), warn = F))
# (raw <- readLines(file.path("input", "input01.txt"), warn = F))
# (raw <- readLines(file.path("input", "input15.txt"), warn = F))

# This function breaks down the string into individual letters
clean_string <- function (i, type) {
  (df <- raw[i] %>% 
     str_split("") %>% 
     unlist() %>% 
     as.data.frame() %>% 
     rename("letter" = ".") %>% 
     mutate(type = type,
            letter = as.character(letter)))
}

# Clean all strings
(s1 <- clean_string(i = 1, type = "s1"))
(s2 <- clean_string(i = 2, type = "s2"))

# These are two types of letters
# Letters that only appear in one of the strings. They need to be deleted.
# Letters that are common to both strings but the lengths are not the same.
# The number of deletions for the second type equals the max number of letters in one string
# less the min letters in the other string.

# Determine a list of common letters
(common_letters <- s1 %>% 
    select(-type) %>% 
    inner_join(s2 %>% 
                 select(-type), by = c("letter")) %>% 
    distinct())

# Determine a list of letters that are not common in both strings then count all elements
# The value is the number of deletions for letters that only appear in either string
(single_letters <- s1 %>% 
    bind_rows(s2) %>% 
    anti_join(common_letters, by = c("letter")) %>% 
    arrange(type, letter) %>% 
    count() %>% 
    pull())

# Determine a list of letters that are common.
# Determine the number of deletions for letters that do not have the same frequency across both strings
(common_but_asymmetric <- s1 %>% 
    bind_rows(s2) %>%
    inner_join(common_letters, by = c("letter")) %>% 
    arrange(type, letter) %>% 
    # Count the number of letters in each string
    group_by(type, letter) %>% 
    summarise(n = n()) %>% 
    # For each letter, determine its the min and max
    group_by(letter) %>% 
    mutate(min = min(n),
           max = max(n)) %>% 
    ungroup() %>% 
    select(-type, -n) %>% 
    distinct() %>% 
    # Determine the difference between max and min
    mutate(difference = max - min) %>% 
    ungroup() %>% 
    summarise(sum(difference)) %>% 
    pull())

# The number of deletions is the sum of single letters plus extras letters for the common letters
(n_deletions <- single_letters + common_but_asymmetric)


