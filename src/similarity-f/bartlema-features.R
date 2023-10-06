# Function generates a stimulus-feature matrix from the experiment 
# in Bartlema et al 2014.
bartlema_features <- function (design = "Diagonal") {
  path <- paste(c("data/matlab-files/bartlema", design, ".mat"), collapse = "")
  
  x <- R.matlab::readMat(con = path)
  
  output <- dplyr::tibble(
    "stimulus" = seq(1, length(unlist(x$d[,,1]$stimulusLabels))),
    "stimulus_name" = unlist(x$d[,,1]$stimulusLabels),
    "feature_1" = x$d[,,1]$p[,1],
    "feature_2" = x$d[,,1]$p[,2])
  
  output$stimulus_name[10] <- "10B"
  
  readr::write_csv(x = output, 
                   file = paste(c("data/stimulus-features/bartlema-", 
                                  design, "-features.csv"), collapse = ""))
}
