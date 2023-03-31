# Function that reads and writes data from one or more conditions in
# Lewandowsky (2011), file is whiten in long-format as a .csv into the 
# ~/data/csv-files directory. (requires the file: 
# data/csv-files/lewandowsky-categories.csv)
lewandowsky_rw <- function(file_suffix, 
                           conditions_keep = c(1:4), 
                           participants_keep = c(1:113)){
  cats <- readr::read_csv(file = "data/csv-files/lewandowsky-categories.csv")
}