# Author: Alex Liu
# Last modified: 2020-01-12
# Description: This script determines the fine towards the borrorwer, if any, based on a book's return date and due date

# Setup -------------------------------------------------------------------

remove(list = ls())

dir <- "/Users/ahtlau/Library/Mobile Documents/com~apple~CloudDocs/Data/HackerRank/Library fine"
setwd(dir)

library(dplyr)
library(stringr)
library(lubridate)

# Analysis ----------------------------------------------------------------

# Load data
(raw <- readLines(file.path("input", "input01.txt"), warn = F))
# (raw <- readLines(file.path("input", "input02.txt"), warn = F))
# (raw <- readLines(file.path("input", "input03.txt"), warn = F))
# (raw <- readLines(file.path("input", "input04.txt"), warn = F))
# (raw <- readLines(file.path("input", "input05.txt"), warn = F))

# Write a function that cleans raw data
clean_string <- function (i) {
  (raw[i] %>% 
     str_replace_all(" ", "-") %>% 
     dmy())
}

# Extract dates
(return_date <- clean_string(i = 1))
(due_date <- clean_string(i = 2))

# Calculate fine
if (return_date <= due_date) {
  (fine <- 0)
} else if (return_date > due_date & 
           year(return_date) == year(due_date) & 
           month(return_date) == month(due_date)) {
  (diff_days <- difftime(return_date, due_date, units = "days") %>% 
     as.double())
  (fine <- diff_days * 15)
} else if (return_date > due_date & 
           year(return_date) == year(due_date)) {
  (diff_months <- month(return_date) - month(due_date) %>% 
     as.double())
  (fine <- diff_months * 500)
} else if (return_date > due_date &
           year(return_date) > year(due_date)) {
  (fine <- 10000)
}




