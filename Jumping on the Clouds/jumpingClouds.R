# Author: Alex Liu
# Last modified: 2019-12-27
# Description: This script finds the minimum of jumps Emma needs to take to win the jumping clouds game

# Setup -------------------------------------------------------------------

remove(list = ls())

dir <- "/Users/ahtlau/Library/Mobile Documents/com~apple~CloudDocs/Data/HackerRank/Jumping on the Clouds"
setwd(dir)

library(stringr)
library(dplyr)

# Analysis ----------------------------------------------------------------

gen_min_jump <- function (filename) {
  
  # Load data
  (raw <- readLines(file.path("input", filename), warn = F))
  
  # Value of final position, that is the total number of elements less the intital position
  (final_pos <- raw[1] %>% 
      str_trim(side = "both") %>% 
      as.integer() - 1)
  
  # Clean arrays
  (arrays <- raw[2] %>%
      str_trim(side = "both") %>% 
      strsplit(., " ") %>%
      unlist() %>%
      as.integer())
  
  # Create a vector containing the corresponding values of thunder clouds
  (vector_thunder_cloud <- str_which(arrays[-1], "1"))
  
  x <- 0 # X denotes the value of current position. Assign starting value of 0. 
  jump_vector <- c() # Create an empty vector which will be populated with the values of each jump
  i <- 1 # assign starting vector element position
  
  # To achieve the min number of jumps, Emma would need to take 2 jumps in each turn unless
  # she is unable to do so, in which case, she would take 1 jump only. 
  
  while (x <= final_pos) {
    # Break the loop if the value of current position equals the value of final position
    if (x == final_pos) {
      break
    } else {
      # If current positions is more than 2 jumps away from final position
      if (final_pos - x > 2) {
        # Test if making 2 jumps would land on any of the thunder clouds. 
        # If so, take 1 jump. 2 jumps otherwise. 
        if (any(x + 2 == vector_thunder_cloud) == TRUE) {
          x <- x + 1
          jump_vector[i] <- x
        } else {
          x <- x + 2
          jump_vector[i] <- x
        }
        # If there are 2 jumps away from final position, take 2 jumps. 
      } else if (final_pos - x == 2) {
        x <- x + 2
        jump_vector[i] <- x
        # If there is only 1 jump away from final position, take 1 jump. 
      } else {
        x <- x + 1
        jump_vector[i] <- x
      }
    }
    i <- i + 1
  }
  
  # Save the values of each jump and the number of jumps in a list
  (results <- list(jump_vector = jump_vector, 
                   min_n_jump = length(jump_vector)))
  
  return(results)
  
}

gen_min_jump(filename = "input00.txt")
gen_min_jump(filename = "input01.txt")
gen_min_jump(filename = "input02.txt")
