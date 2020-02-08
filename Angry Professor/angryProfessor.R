# Author: Alex Liu
# Last modified: 2020-02-08
# Description: This script determines if a class is canceled or not depending on students' arrival times

# Setup -------------------------------------------------------------------

remove(list = ls())

library(stringr)
library(dplyr)

dir <- "/Users/ahtlau/Library/Mobile Documents/com~apple~CloudDocs/Data/HackerRank/Angry Professor"
setwd(dir)

# Analysis ----------------------------------------------------------------

# Load data
# (raw <- readLines(file.path("input", "input00.txt"), warn = F))
(raw <- readLines(file.path("input", "input01.txt"), warn = F))

# Find vector that contains all test cases
(arr <- raw[-1])

gen_results <- function (i) {
  
  # Subset test case
  (test_case <- arr[i:(i+1)])
  
  # Find the min attendence required
  (min <- test_case[1] %>% 
    str_split(., " ") %>% 
    unlist() %>% 
    str_trim(side = "both") %>% 
    as.double())

  (min <- min[2])
  
  # Clean arrival time vector
  (arrival_time <- test_case[2] %>% 
    str_split(., " ") %>% 
    unlist() %>% 
    str_trim(side = "both") %>% 
    as.double())
  
  # determine the number of students who arrived on time
  (n <- length((arrival_time[arrival_time <= 0])))
  
  # If attendence is greater or equal to min, class is not cancelled (i.e., "NO")
  if (n >= min) {
    return("NO")
    # If attendence is less than min, class is cancelled (i.e., "YES")
  } else {
    return("YES")
    }
}

# Gen a vector containing the first element of each test case
sequence <- seq(from = 1, to = length(arr), by = 2)

(results <- lapply(sequence, gen_results) %>% unlist())

cat(paste0(noquote(results), collapse = "\n"))


