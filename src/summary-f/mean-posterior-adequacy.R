# Function to calculate posterior adequacy
posterior_adequacy <- function (chmm_path, gcm_path, data) {
  
  n_stimulus <- length(unique(data$stimulus))
  
  trials <- unique(data$trial)
  
  participants <- unique(data$id)
  
  chmm_posterior <- readRDS(file = paste(chmm_path))
  
  gcm_posterior <- readRDS(file = paste(gcm_path))
  
  output <- list()
  
  output$chmm <- array(data = NA, 
                       dim = c(length(participants), 
                               length(trials),
                               n_stimulus))
  
  output$gcm <- array(data = NA, 
                      dim = c(length(participants),
                              length(trials),
                              n_stimulus))
  
  count <- 0
  for (pp in participants) {
    transfer_data <- subset(x = data, subset = id == pp)

    count <- count + 1
    
    for (t in trials) {
      output$chmm[count, t, transfer_data$stimulus[t]] <- 
        transfer_data$response[t] * chmm_posterior[count, t] +
        (1 - transfer_data$response[t]) * (1 - chmm_posterior[count, t])
      
      output$gcm[count, t, transfer_data$stimulus[t]] <- 
        transfer_data$response[t] * gcm_posterior[count, transfer_data$stimulus[t]] +
        (1 - transfer_data$response[t]) * (1 - gcm_posterior[count, transfer_data$stimulus[t]])
      
    }
  }
  return(output)
}
