# Creates a data file in long format that contains the stimulus x condition x
# category label used in Lewandowsky 2011, this file is needed in order to 
# be able to reproduce real time participants responses.
lewandowsky_categories_w <- function(){
  condition <- rep(x = 1:6, each = 8)
  stimulus <- rep(x = 1:8, times = 6)
  category <- c(rep(x = c("gray", "white"), times = 4),
                rep(x = c("gray", "white", "gray"), times = c(2,4,2)),
                rep(x = c("gray", "white", "gray", "white"), 
                    times = c(3,2,1,2)),
                rep(x = c("gray", "white", "gray", "white"), 
                    times = c(3,1,1,3)),
                rep(x = c("gray", "white", "gray"), 
                    times = c(3,4,1)),
                rep(x = c("gray", "white", "gray", "white", "gray", "white"), 
                    times = c(1,2,1,1,2,1)))
  

  out <- dplyr::tibble(condition, stimulus, category) |>
    readr::write_csv(file = "data/lewandowsky-categories.csv")
}
