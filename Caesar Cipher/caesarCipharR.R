# Author: Alex Liu
# Last modified: 2020-02-15
# Description: This script replaces letters with other letters from an adjusted alphabet table

# Setup -------------------------------------------------------------------

remove(list = ls())

library(stringr)
library(dplyr)

dir <- "/Users/ahtlau/Library/Mobile Documents/com~apple~CloudDocs/Data/HackerRank/Caesar Cipher"
setwd(dir)

# Analysis ----------------------------------------------------------------

# Load data
(raw <- readLines(file.path("input", "input05.txt"), warn = F))
# (raw <- readLines(file.path("input", "input11.txt"), warn = F))
# (raw <- readLines(file.path("input", "input12.txt"), warn = F))
# (raw <- readLines(file.path("input", "input13.txt"), warn = F))
# (raw <- readLines(file.path("input", "input14.txt"), warn = F))

# Extract number of shifts and original string
(n_shift <- raw[3] %>% as.double())
(string <- raw[2])

# Calculate net shift
(net_shift <- n_shift %% 26)

# If 0, no shift. 
if (net_shift == 0) {
  (new_string <- string)
} else {
  # Create new alphabet order - lower case
  (new_alphbet_l <- letters[-c(1:net_shift)])
  length <- length(new_alphbet_l)
  new_alphbet_l[(length+1):26] <- letters[c(1:net_shift)]
  
  # Create new alphabet order - upper case
  (new_alphbet_u <- LETTERS[-c(1:net_shift)])
  length <- length(new_alphbet_u)
  new_alphbet_u[(length+1):26] <- LETTERS[c(1:net_shift)]
  
  # Create a mapping
  mapping <- data.frame(og_l = letters,
                        new_l = new_alphbet_l,
                        og_u = LETTERS,
                        new_u = new_alphbet_u,
                        stringsAsFactors = F)
  
  # Map new alphabet order (lower and upper cases) to the original string
  (new_string <- data.frame(og = unlist(str_split(string, "")),
                            stringsAsFactors = F) %>% 
      left_join(mapping %>% select(og_l, new_l), by = c("og" = "og_l")) %>% 
      left_join(mapping %>% select(og_u, new_u), by = c("og" = "og_u")) %>% 
      # Create new string
      mutate(new = if_else(is.na(new_u), new_l, new_u),
             new = if_else(is.na(new), og, new)) %>% 
      pull(new) %>% 
      paste0(., collapse = ""))
}