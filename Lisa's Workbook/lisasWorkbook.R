# Author: Alex Liu
# Last modified: 2020-02-16
# Description: This script finds questions whose number equals page number

# Setup -------------------------------------------------------------------

remove(list = ls())

library(stringr)
library(dplyr)

dir <- "/Users/ahtlau/Library/Mobile Documents/com~apple~CloudDocs/Data/HackerRank/Lisa's Workbook"
setwd(dir)

# Analysis ----------------------------------------------------------------

# Load data
(raw <- readLines(file.path("input", "input00.txt"), warn = F))
# (raw <- readLines(file.path("input", "input01.txt"), warn = F))

# Clean arrays
clean <- function (i) {
  temp <- str_split(raw[i], " ") %>%
    unlist() %>% 
    str_trim(side = "both") %>% 
    as.double()
}

# Extract max questions in a page (k) and array
(n <- clean(i = 1)[1])
(k <- clean(i = 1)[2])
(arr <- clean(i = 2))

dfs <- lapply(1:n, function (i) {data.frame(number = 1:arr[i], chapter = i)})

# Assign each question to groups
(df <- bind_rows(dfs) %>% 
    # For each chapter, determine quotient which is indicative of the page number it belongs to
    group_by(chapter) %>% 
    mutate(quotient = floor(number / k),
           q_lagged = lag(quotient, 1),
           q_lagged = if_else(is.na(q_lagged), 0, q_lagged),
           flag = paste0(chapter, "_", q_lagged)) %>% 
    ungroup())

# Create a mapping of group and page number
(mapping <- df %>% 
  select(flag) %>% 
  distinct() %>% 
  mutate(page_n = 1:n()))

# Join mapping and determine questions whose number equals page number
(df_page <- df %>% 
  left_join(mapping, by = c("flag")) %>% 
  mutate(test = page_n == number) %>% 
  pull(test) %>% 
  sum())






