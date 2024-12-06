setwd("C:/Users/op000006/OneDrive - Defra/L&D/AoC_2024")

library(readr)
library(dplyr)
library(stringr)

input <- read_lines("input02.txt")

# for each part of input

safe_reports <- c()

for (i in 1:length(input)) {
  
  levels <- as.numeric(unlist(str_split(input[i], " ")))
  differences <- c()
  
  for (j in 2:length(levels)) {
    
    difference <- levels[j] - levels[j-1]
    differences <- c(differences, difference)
    
  }
  
  if (all(differences > 0) | all(differences < 0)) {
    
    abs_differences <- abs(differences)
    
    if (all(abs_differences %in% (1:3))) {
      
      safe_reports <- c(safe_reports, i)
      
    }
  
  }

}

# answer part 1
length(safe_reports)


# part 2 ------------------------------------------------------------------

safe_reports_2 <- c()

for (i in 1:length(input)) {
  
  levels <- as.numeric(unlist(str_split(input[i], " ")))
  differences <- c()
  
  for (j in 2:length(levels)) {
    
    difference <- levels[j] - levels[j-1]
    differences <- c(differences, difference)
    
  }
  
  if (all(differences > 0) | all(differences < 0)) {
    
    abs_differences <- abs(differences)
    
    if (all(abs_differences %in% (1:3))) {
      
      safe_reports_2 <- c(safe_reports_2, i)
      
    } else 
      
      for (k in 1:length(levels)) {
        
        new_levels <- levels[-k]
        new_differences <- c()
        
        for (l in 2:length(new_levels)) {
          
          new_difference <- new_levels[l] - new_levels[l-1]
          new_differences <- c(new_differences, new_difference)
          
        }
        
        if (all(new_differences > 0) | all(new_differences < 0)) {
          
          abs_new_differences <- abs(new_differences)
          
          if (all(abs_new_differences %in% (1:3))) {
            
            safe_reports_2 <- c(safe_reports_2, i)
            
          }
          
        }
        
      }
    
  }
  
}


length(safe_reports_2)






                 