# Transform Bartlema's diagonal data to be used by the GCM

transform_gcm <- function (data_path, features_path, transfer_id) {
  output <- list()
  
  if (!missing(transfer_id)) {
    data <- readr::read_csv(file = data_path)
    
    transfer_trials <- unique(data$trial_condition[which(data$condition_char == "transfer")])
    
    data <- subset(data, subset = trial_condition < transfer_trials[transfer_id])
  }
  else {
    data <- readr::read_csv(file = data_path)
  }
  
  features <- readr::read_csv(file = features_path)
  
  output$n_participants <- length(unique(data$id))
  
  output$n_stimulus <- dim(features)[1]
  
  output$n_dimentions <- dim(features)[2] - 2
  
  output$belong_a <- as.numeric(grepl(pattern = "A", x = features$stimulus_name))
  
  output$belong_b <- as.numeric(grepl(pattern = "B", x = features$stimulus_name))
  
  output$features <- features[, 3:4]
  
  output$n_trials <- unname(
    obj = table(data$stimulus[which(data$id == unique(data$id)[1])]))
  
  if (!missing(transfer_id)) {
    if (transfer_id == 1) {
      output$n_trials <- output$belong_a * unique(output$n_trials) +
        output$belong_b * unique(output$n_trials)
    }
  }
  
  output$y <- matrix(data = NA, 
                     nrow = output$n_participants, 
                     ncol = output$n_stimulus)
  
  for (i in 1:output$n_participants) {
    for(j in 1:output$n_stimulus)
    output$y[i, j] <- sum(
      subset(x = data, 
             subset = id == unique(data$id)[i] & stimulus == j)$response_char == "A")
  }
  
  return(output)
}
