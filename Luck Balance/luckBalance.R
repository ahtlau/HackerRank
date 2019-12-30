# Author: Alex Liu
# Last modified: 2019-12-29
# Description: This script finds the maximum luck balance for Lena

# Setup -------------------------------------------------------------------

remove(list = ls())

dir <- "/Users/ahtlau/Library/Mobile Documents/com~apple~CloudDocs/Data/HackerRank/Luck Balance"
setwd(dir)

library(stringr)
library(dplyr)

# Analysis ----------------------------------------------------------------

# Load data
(raw <- readLines(file.path("input", "input00.txt"), warn = F))
# (raw <- readLines(file.path("input", "input03.txt"), warn = F))
# (raw <- readLines(file.path("input", "input12.txt"), warn = F))
# (raw <- readLines(file.path("input", "input13.txt"), warn = F))
# (raw <- readLines(file.path("input", "input14.txt"), warn = F))
# (raw <- readLines(file.path("input", "input15.txt"), warn = F))

# Write a function to clean data
clean_data <- function (i) {
  raw[i] %>% 
    str_split(" ") %>% 
    unlist() %>% 
    as.double()
}

# Call clean function to different arrays
(n_rows_k <- clean_data(i = 1))
(arrays <- clean_data(i = -1))

# Extract points
(n_rows <- n_rows_k[1])
(k <- n_rows_k[2]) 
# k is the maximum number of important contests Lena can lose
# hence the number of important contests Lena has to win is the total less k
# E.g. if k is 3 and total important contests is 4, Lena needs to win 1 contest

# Make arrays a matrix
(matrix <- matrix(arrays, nrow = n_rows, byrow = T))

# Make matrix a data frame
(df <- as.data.frame(matrix) %>% 
    rename("luck" = V1,
           "importance" = V2) %>% 
    mutate(contest_id = row_number()))

# Count the number of important cases (denoted as 1)
(total_important_contests <- df %>% 
    filter(importance == 1) %>% 
    nrow())

# Calculate the number of contests that Lena needs to win
(n_contests_to_win <- total_important_contests - k)

# If the number of contests Lena is allowed to lose is greater than the number of important contests
# this will results in a negative number
# in this case, simply sum up all of the luck scores
if (n_contests_to_win < 0) {
  (sum <- df %>% 
     select(luck) %>% 
     summarise(sum = sum(luck)) %>% 
     pull())
} else {
  # filter on the important contests Lena needs to win
  # Her aim is to win the ones that would give her the min luck score
  (contest_to_win <- df %>% 
     arrange(desc(importance), luck) %>% 
     filter(importance == 1) %>%
     slice(1:n_contests_to_win))
  
  # Summarize the luck scores for the ones Lena has to win
  (luck_lost <- contest_to_win %>% 
      select(luck) %>% 
      summarise(sum = sum(luck)) %>% 
      pull())
  
  # Summarize the luck score for the ones Lena will lose
  (luck_won <- df %>% 
      anti_join(contest_to_win, by = c("contest_id")) %>% 
      select(luck) %>% 
      summarise(sum = sum(luck)) %>% 
      pull(sum))
  
  # Calculate net luck scores
  (sum <- luck_won - luck_lost)
}
