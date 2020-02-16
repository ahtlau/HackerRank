# Author: Alex Liu
# Last modified: 2020-02-15
# Description: This script determines the area of a given string

# Setup -------------------------------------------------------------------

remove(list = ls())

library(stringr)
library(dplyr)

dir <- "/Users/ahtlau/Library/Mobile Documents/com~apple~CloudDocs/Data/HackerRank/Designer PDF Viewer"
setwd(dir)

# Analysis ----------------------------------------------------------------

# Load data
(raw <- readLines(file.path("input", "input00.txt"), warn = F))
# (raw <- readLines(file.path("input", "input06.txt"), warn = F))

# Create a mapping of height and letter
mapping <- data.frame(height = as.double(unlist(str_split(raw[1], " "))),
                      letter = letters,
                      stringsAsFactors = F)

# Split word string into letters and determine length
(split_letters <- str_split(raw[2], "") %>% unlist())
(length_letter <- length(split_letters))

# Find the max height of all the letters
(max_height <- mapping %>% 
  filter(letter %in% split_letters) %>% 
  pull(height) %>% 
  max)

# Calculate area
(area <- max_height * length_letter)
