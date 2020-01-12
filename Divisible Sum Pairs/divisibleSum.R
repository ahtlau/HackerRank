# Author: Alex Liu
# Last modified: 2020-01-12
# Description: This script calculates the number of pairs of numbers in an array whose sum is divisible by a given divisor

# Setup -------------------------------------------------------------------

remove(list = ls())

dir <- "/Users/ahtlau/Library/Mobile Documents/com~apple~CloudDocs/Data/HackerRank/Divisible Sum Pairs"
setwd(dir)

library(dplyr)
library(stringr)

# Analysis ----------------------------------------------------------------

# Load data
(raw <- readLines(file.path("input", "input00.txt"), warn = F))
# (raw <- readLines(file.path("input", "input12.txt"), warn = F))

# Extract and clean array
clean_string <- function (i) {
  (raw[i] %>% 
     str_split(" ") %>% 
     unlist() %>% 
     str_trim(side = "both") %>% 
     as.double())
}

# Call function to each string
(divisor <- clean_string(i = 1)[2])
(numbers <- clean_string(i = 2))

# Length of arrays
(len <- length(numbers))

# Assign indice to each elements 
(names(numbers) <- 1:len)

# Generate a data frame that contains the cartesian product of the numbers in long format,
# plus the corresponding indices of each number
(df <- data.frame(i = rep(numbers, each = len),
                  j = rep(numbers, len),
                  i_indice = rep(names(numbers), each = len),
                  j_indice = rep(names(numbers), len)))

# Determine the number of pairs that is divisible by the given divisor
(n <- df %>% 
    mutate(i_indice = as.double(i_indice),
           j_indice = as.double(j_indice)) %>% 
    filter(i_indice < j_indice) %>% 
    select(i:j) %>% 
    mutate(sum = i + j,
           remainder = sum %% divisor) %>% 
    filter(remainder == 0) %>% 
    count() %>% 
    pull(n))

