# Author: Alex Liu
# Last modified: 2020-02-15
# Description: This script determines the number of letters which is different from expected SOS signal

# Setup -------------------------------------------------------------------

remove(list = ls())

library(stringr)
library(dplyr)

dir <- "/Users/ahtlau/Library/Mobile Documents/com~apple~CloudDocs/Data/HackerRank/Mars Exploration"
setwd(dir)

# Analysis ----------------------------------------------------------------

# Load data
# (raw <- readLines(file.path("input", "input00.txt"), warn = F))
# (raw <- readLines(file.path("input", "input01.txt"), warn = F))
(raw <- readLines(file.path("input", "input11.txt"), warn = F))

# Break received message into individual letters
(string_received <- unlist(str_split(raw, "")))

# Determine the number of SOSs that the message should have
(n_sos <- length(string_received) / 3)

# Recreate the original message
(string_expected <- rep("SOS", n_sos) %>% 
    paste0(., collapse = "") %>% 
    str_split(., "") %>%
    unlist())

# Compare received and expected messages
(n <- table(string_received == string_expected)["FALSE"] %>% as.double())

# Assign 0 if they are the same 
if (is.na(n)) {n <- 0}

n

