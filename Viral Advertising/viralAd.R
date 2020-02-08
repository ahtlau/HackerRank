# Author: Alex Liu
# Last modified: 2020-02-08
# Description: This script determines the total number of people who has seen an ad on the nth day after an ad campaign has launched

# Setup -------------------------------------------------------------------

remove(list = ls())

library(stringr)
library(dplyr)

dir <- "/Users/ahtlau/Library/Mobile Documents/com~apple~CloudDocs/Data/HackerRank/Viral Advertising"
setwd(dir)

# Analysis ----------------------------------------------------------------

# Extract nth day
(nth_day <- readLines(file.path("input", "input00.txt"), warn = F) %>% as.double)
# (nth_day <- readLines(file.path("input", "input01.txt"), warn = F) %>% as.double)
# (nth_day <- readLines(file.path("input", "input02.txt"), warn = F) %>% as.double)

# Assign # of people to which the ad is shared at the beginning
(shared <- c(5))

# Create a vector which contains the # of people who liked the ad in each day
(liked <- c())

# Assign initial day
i = 1

while (i <= nth_day) {
  # # of likes on an ith day equals to the floor of # of people who saw the ad divided by 2
  liked[i] <- floor(shared[i] / 2)
  # Move onto the next day
  i <- i + 1
  # # of people who see the ad next day equals the # of people who saw the ad from yesterday times 3
  shared[i] <- liked[i-1] * 3
  # Stop the loop if the ith day is greater than the nth_day 
  if (i > nth_day) {
    break
  }
}

sum(liked)
