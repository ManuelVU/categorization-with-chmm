# This function takes generates a stimulus feature matrix from the experiment 
# in Lee and Navarro 2002
lee_navarro_features <- function(){
  stimulus_name <- apply(X = cbind(
    rep(x = c("circle-", "square-", "triangle-"), each = 3),
    rep(x = c("red", "green", "blue"), times = 3)), 
    MARGIN = 1, FUN = paste, collapse = "")
  
  features <- dplyr::tibble(stimulus = seq(1,9),
                            stimulus_name,
                            "circle" = c(rep(x = 1, times = 3), 
                                         rep(x = 0, times = 6)),
                            "triangle" = c(rep(x = c(0, 1), each = 3), 
                                           rep(x = 0, times = 3)),
                            "square" = c(rep(x = 0, times = 6), 
                                         rep(x = 1, times = 3)),
                            "red" = rep(x = c(1, 0, 0), times = 3),
                            "green" = rep(x = c(0, 1, 0), times = 3),
                            "blue" = rep(x = c(0, 0, 1), times = 3))
  
  readr::write_csv(x = features, 
                   file = "data/stimulus-features/lee-navarro-features.csv")
}
