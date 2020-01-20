# Author: Alex Liu
# Last modified: 2020-01-20
# Description: This script tests whether there exists a point in the array 
# where the sum of the subarray below this point equals the sum of the subarray above this point

# Setup -------------------------------------------------------------------

remove(list = ls())

dir <- "/Users/ahtlau/Library/Mobile Documents/com~apple~CloudDocs/Data/HackerRank/Sherlock and Array"
setwd(dir)

start_time <- Sys.time()

library(stringr)
library(dplyr)

# Analysis ----------------------------------------------------------------

# Load data
# (raw <- readLines(file.path("input", "input05.txt"), warn = F))
# (raw <- readLines(file.path("input", "input07.txt"), warn = F))
raw <- readLines(file.path("input", "input08.txt"), warn = F)

# (n_arr <- raw[1] %>% as.double()) 
n_arr <- raw[1] %>% as.double()

# (arrs <- raw[seq(from = 3, to = length(raw), by = 2)])
arrs <- raw[seq(from = 3, to = length(raw), by = 2)]

test_arr <- function (i) {
  (arr_clean <- arrs[i] %>% 
     str_trim(side = "both") %>% 
     str_split(., " ") %>% 
     unlist() %>% 
     as.double() %>% 
     as.data.frame() %>% 
     rename("arr" = ".") %>% 
     mutate(row_name = row_number()))
  
  # Cumsum arrs in ascending and descening order
  # If there exists such a point, sum_1 = sum_2
  (n_row <- arr_clean %>% 
      mutate(sum_1 = cumsum(arr)) %>% 
      full_join(arr_clean %>% 
                  arrange(desc(row_name)) %>% 
                  mutate(sum_2 = cumsum(arr)),
                by = c("row_name")) %>% 
      filter(sum_1 == sum_2) %>% 
      nrow())

  if (n_row >= 1) {
    result_saved <- "YES"
  } else {
    result_saved <- "NO"
  }
  
  return(result_saved)
}

(results <- lapply(1:length(arrs), test_arr) %>% unlist())

# Print results vertically
cat(paste(results, collapse = '\n'))

end_time <- Sys.time()

end_time - start_time


