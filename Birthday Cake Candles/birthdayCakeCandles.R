# Author: Alex Liu
# Last modified: 2020-01-20
# Description: This script determines the maximum number of candles the niece can blow 

# Setup -------------------------------------------------------------------

remove(list = ls())

library(stringr)
library(dplyr)

dir <- "/Users/ahtlau/Library/Mobile Documents/com~apple~CloudDocs/Data/HackerRank/Birthday Cake Candles"
setwd(dir)

# Analysis ----------------------------------------------------------------

# Load data
(raw <- readLines(file.path("input", "input00.txt"), warn = F))

# Get max number of candles
(arr <- raw[2] %>% 
    str_trim(side = "both") %>% 
    str_split(., " ") %>% 
    unlist() %>% 
    as.double() %>% 
    as.data.frame() %>% 
    rename("arr" = ".") %>% 
    filter(arr == max(arr)) %>% 
    nrow())


