# Author: Alex Liu
# Last modified: 2019-12-30
# Description: This script determines if Anna is overcharged for a restaurant bill she shared with Brian

# Setup -------------------------------------------------------------------

remove(list = ls())

dir <- "/Users/ahtlau/Library/Mobile Documents/com~apple~CloudDocs/Data/HackerRank/Bon Appétit"
setwd(dir)

library(dplyr)
library(stringr)

# Analysis ----------------------------------------------------------------

# Load data
(raw <- readLines(file.path("input", "input00.txt"), warn = F))
# (raw <- readLines(file.path("input", "input06.txt"), warn = F))

# Extract and clean array
clean_string <- function (i) {
  raw[i] %>% 
    str_split(" ") %>% 
    unlist() %>% 
    str_trim(side = "both") %>% 
    as.double()
}

# Call function to clean each string
(non_sharing_item <- clean_string(i = 1)[2]) # zero_based index
(costs_array <- clean_string(i = 2))
(b_charged <- clean_string(i = 3))

# Remove non-sharing item from cost array
(shared_costs_arrays <- costs_array[-(non_sharing_item + 1)])

# Split costs of shared items
(b_actual <- sum(shared_costs_arrays)/2)

if (b_actual != b_charged) {
  (result <- b_charged - b_actual)
} else {
  (result <- noquote("Bon Appetit"))
}

result




