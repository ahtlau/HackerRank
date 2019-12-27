# Author: Alex Liu
# Last modified: 2019-12-27
# Description: This scripts determines the number of paired socks that can be sold

# Setup -------------------------------------------------------------------

library(dplyr)
library(stringr)

dir <- "/Users/ahtlau/Library/Mobile Documents/com~apple~CloudDocs/Data/HackerRank/Sock Merchant"

setwd(dir)

# Analysis --------------------------------------------------------------------

# Load file
# (df <- readLines(file.path("input", "input00.txt"), warn = F))
(df <- readLines(file.path("input", "input08.txt"), warn = F))

# Extract n and arrays
(n_total <- df[1] %>% as.double())
(ar <- df[2] %>% strsplit(., " ") %>% unlist() %>% as.double())

# Tabulate count of each colour and make it a dataframe
(df <- as.data.frame(table(ar)))

# If freq is an odd number, this means some of the socks are unmatched. 
# If freq is an even number, this means all of the socks are matched. 
# Divide freq by 2, The remainder, 1, is the number of socks unmatched.
# Sum up all the 1's to get the number of unmatched socks
(n_unmatched_socks <- df %>% 
  mutate(remainder = Freq %% 2) %>% 
    summarise(sum(remainder)) %>% 
    pull())

# Number of socks that can be sold is total less the number of unmatched socks divided by 2
(n_pairs <- (n_total - n_unmatched_socks) / 2)


