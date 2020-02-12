# Author: Alex Liu
# Last modified: 2020-02-12
# Description: This script determines the height of a tree after n periods

# Setup -------------------------------------------------------------------

remove(list = ls())

library(stringr)
library(dplyr)

dir <- "/Users/ahtlau/Library/Mobile Documents/com~apple~CloudDocs/Data/HackerRank/Utopian tree"
setwd(dir)

# Analysis ----------------------------------------------------------------

# Load data
(raw <- readLines(file.path("input", "input00.txt"), warn = F))
# (raw <- readLines(file.path("input", "input01.txt"), warn = F))

# Clean array
(arr <- raw %>% 
    str_split(" ") %>% 
    unlist() %>% 
    str_trim(side = "both") %>% 
    as.double())

# Extract number of cases
(n_cases <- arr[1])

# Extract test cases
(arr <- arr[-1])

(intial_period <- 0)
height <- c(1)
j <- 1

gen_result <- function (i) {
  # Extract end period from array
  end_period <- arr[i]
  while (j <= end_period) {
    # if j is an odd number (i.e. summer), height is double
    if (j %% 2 > 0) {
      height[j + 1] <- height[j] * 2
      j <-  j + 1
      # if j is an even number (i.e. spring), height grows by 1
    } else {
      height[j + 1] <- height[j] + 1
      j <- j + 1
    }
    # Break if reached at the end of the period
    if (j > end_period) {
      break
    }
  }
  return(height[end_period + 1])
}

(results <- lapply(1:n_cases, gen_result) %>% unlist())






