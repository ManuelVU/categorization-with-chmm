# Function that calculates the posterior predictive accuracy of the chmm and gcm
# models for the four transfer conditions used in figure ...

posterior_pred_accuracy <- function (chmm_path, gcm_path, data) {
  transfer_id <- c(1, 3, 5, 7)
  
  transfer_trials <- unique(data$trial_condition[data$condition_char == "transfer"])
  
  n_stimulus <- length(unique(data$stimulus))
  
  participants <- unique(data$id)
  
  output <- list()
  
  output$chmm <- array(data = NA, 
                       dim = c(length(participants),
                               length(transfer_id),
                               n_stimulus))
  
  output$gcm <- array(data = NA, 
                      dim = c(length(participants),
                              length(transfer_id),
                              n_stimulus))
  
  count_transfer <- 0
  
  for (i in transfer_id) {
    count_transfer <- count_transfer + 1
    
    chmm_posterior <- readRDS(file = paste(c(chmm_path, i, ".rds"), 
                                           collapse = ""))$y_hat
    
    gcm_posterior <- readRDS(file = paste(c(gcm_path, i, ".rds"), 
                                          collapse = ""))
    
    count <- 0
    
    for (pp in participants) {
      count <- count + 1
      
      transfer_data <- subset(x = data, subset = id == pp & 
                                        trial_condition == transfer_trials[i])
      
      transfer_data <- transfer_data[order(transfer_data$stimulus, 
                                           decreasing = FALSE), ]
      
      output$chmm[count, count_transfer, ] <- 
        transfer_data$response * chmm_posterior[count, ] +
        (1 - transfer_data$response) * (1 - chmm_posterior[count, ])
      
      output$gcm[count, count_transfer, ] <- 
        transfer_data$response * gcm_posterior[count, ] +
        (1 - transfer_data$response) * (1 - gcm_posterior[count, ])
      
    }
  }
  return(output)
}
