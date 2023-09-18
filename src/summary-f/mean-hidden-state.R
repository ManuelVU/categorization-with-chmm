# Function takes the posterior sample of the CHMM and returns a matrix with the 
# mean state by stimulus (rows) and trial (columns) for a single participant

mean_hidden_state <- function (posterior, participant, total_trials) {
  
  hidden <- posterior$posterior_samples$hidden_states
  
  mean_hs <- apply(X = hidden[,,participant,], MARGIN = c(1,2), 
                   FUN = mean, na.rm = TRUE)
  
  mean_hs <- mean_hs[, 1:total_trials]
  
  return(mean_hs)
}

