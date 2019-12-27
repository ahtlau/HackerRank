# Author: Alex Liu
# Last modified: 2019-12-27
# Description: This scripts finds the maximum number of toys one can buy given a budget

# Setup -------------------------------------------------------------------

remove(list = ls())

library(stringr)
library(dplyr)

dir <- "/Users/ahtlau/Library/Mobile Documents/com~apple~CloudDocs/Data/HackerRank/Mark and toys"
setwd(dir)

# Analysis ----------------------------------------------------------------

# Load data
# (raw <- readLines(file.path("input", "input00.txt"), warn = F))
# (raw <- readLines(file.path("input", "input16.txt"), warn = F))
# (raw <- readLines(file.path("input", "input17.txt"), warn = F))
# (raw <- readLines(file.path("input", "input18.txt"), warn = F))
(raw <- readLines(file.path("input", "input19.txt"), warn = F))

# Extract budget 
(budget <- raw[1] %>% 
  str_split(" ") %>% 
  unlist() %>% 
  as.integer())

(budget <- budget[2])

# Sort prices in ascending order
(prices <- raw[2] %>% 
    str_split(" ") %>% 
    unlist() %>% 
    as.numeric() %>% 
    sort())

# Calcualte cumulative costs if one were to buy multiple toys starting with the cheapest
(cumsum_costs <- cumsum(prices))

# compare cumulative costs with budget
(test_vector <- cumsum_costs < budget)

# Filter toys using previous vector 
(cumsum_costs_filtered <- cumsum_costs[test_vector])

# Calculate number of toys
(toy <- length(cumsum_costs_filtered))




