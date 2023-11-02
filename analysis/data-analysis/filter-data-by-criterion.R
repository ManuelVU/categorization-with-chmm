# Evaluate participant's behavior and create a data files with only those who 
# reach criterion
source("src/data-f/lee-navarro-rw.R")

source("analysis/performance-evaluation/learning-rate.R")
source("analysis/performance-evaluation/total-trials-lee.R")

lee_navarro_rw(file_suffix = "type1-filtered", conditions_keep = 1, 
               participants_keep = participants_to_keep_lee)

lee_navarro_rw(file_suffix = "type4-filtered", conditions_keep = 4, 
               participants_keep = participants_to_keep_lee)
