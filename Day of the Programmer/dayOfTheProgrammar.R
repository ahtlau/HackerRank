# Author: Alex Liu
# Last modified: 2020-02-08
# Description: This script determines the 256th day in Russia from 1700 to 2700

# Setup -------------------------------------------------------------------

remove(list = ls())

library(stringr)
library(dplyr)

dir <- "/Users/ahtlau/Library/Mobile Documents/com~apple~CloudDocs/Data/HackerRank/Day of the Programmer"
setwd(dir)

# Analysis ----------------------------------------------------------------

# Load data
(year <- readLines(file.path("input", "input00.txt"), warn = F) %>% as.double())
# (year <- readLines(file.path("input", "input01.txt"), warn = F) %>% as.double())
# (year <- readLines(file.path("input", "input60.txt"), warn = F) %>% as.double())
# (year <- readLines(file.path("input", "input61.txt"), warn = F) %>% as.double())
# (year <- readLines(file.path("input", "input62.txt"), warn = F) %>% as.double())

# Assign the month and day of the 256th day of the year for leap and non-leap years per documentation
leap <- "12.09."
non_leap <- "13.09."

# Test if the year belongs to Julian, Georgian or transition year
if (between(year, 1700, 1917) == TRUE) {
  # If Julian, test if the year is a leap year or not
  if (year %% 4 == 0) {
    date <- paste0(leap, year)
  } else {
    date <- paste0(non_leap, year)
  }
} else if (year == 1918) {
  # Calculate the difference between Julian and Georgian calander per info provided in documentation
  diff <- as.double(as.Date("1918-02-14") - as.Date("1918-02-01"))
  # If 1918, which is not a leap year, add the difference to the non-leap year date 
  day_non_leap <- 13 + diff
  date <- paste0(day_non_leap, ".09.", year)
} else if (between(year, 1919, 2700) == TRUE) {
  # If Georgian, test if the year is a leap year or not
  if (((year %% 4 == 0 & year %% 100 > 0) | year %% 400 == 0) == TRUE) {
    date <- paste0(leap, year)
  } else {
    date <- paste0(non_leap, year)
  }
}

noquote(date)


