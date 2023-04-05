# This function takes generates a stimulus feature matrix from the experiment 
# in Lewandowsky 2011
lewandowsky_features <- function(){
  stimulus_name <- apply(X = cbind(
    rep(x = c("gray-", "white-"), each = 4), 
    rep(x = c("triangle-", "square-"), times = 4),
    rep(x = c("large", "small", "large","small"), each = 2)),
    MARGIN = 1, FUN = paste, collapse = "")
  
  features <- dplyr::tibble(stimulus = as.integer(seq(1,8)),
                            stimulus_name,
                            "gray" = rep(x = c(1, 0), each = 4),
                            "white" = rep(x = c(0, 1), each = 4), 
                            "triangle" = rep(x = c(1,0), times = 4),
                            "square" = rep(x = c(0,1), times = 4),
                            "large" = rep(x = c(1, 1, 0, 0), 
                                          times = 2),
                            "small" = rep(x = c(0, 0, 1, 1), 
                                          times = 2))
  
  readr::write_csv(x = features, 
                   file = "data/stimulus-features/lewandowsky-features.csv")
}
