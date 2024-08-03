################################################################################
# Function that samples from the posterior predictive distribution of a single
# participant with a fixed experimental design.
# The function returns a matrix where the rows index the iteration number and 
# the columns index the trial number
################################################################################

# This function takes 5 arguments
#   1: iterations, number of posterior samples to take.
#   2: posterior, posterior samples of the parameters in the model
#   3: stimulus_id, id of the stimulus presented at each trial.
#   4: participant_id, id of the participant to sample.
#   5: total_trials, total number of trials completed by participant in the 
#      experiment.

posterior_adequacy <- function (iterations, 
                                posterior, 
                                stimulus_id, 
                                participant_id,
                                total_trials) {
  
  n_posterior_samples <- dim(posterior$posterior_samples$epsilon)[1]
  
  if (missing(iterations)) {
    iterations <- seq(1, n_posterior_samples)
  }
  else if (iterations > n_posterior_samples){
    
    iterations <- sample(seq(1, n_posterior_samples), size = iterations,
                         replace = TRUE)    
  }
  else {
    iterations <- sample(seq(1, n_posterior_samples), size = iterations,
                         replace = FALSE)
  }
  
  epsilon <- posterior$posterior_samples$epsilon[, participant_id]
  
  hidden_states <- posterior$posterior_samples$hidden_states[, 1:total_trials,
                                                             participant_id, ]
  
  y_hat <- matrix(NA, nrow = length(iterations), ncol = total_trials)
  
  position <- 0
  
  for (i in iterations) {
    position <- position + 1
    
    for (t in 1:total_trials) {
      y_hat[position, t] <- rbinom(n = 1, size = 1,
                                   prob = hidden_states[stimulus_id[t], t, i] * 
                                     (1 - epsilon[i]) + 
                                     (1 - hidden_states[stimulus_id[t], t, i]) * 
                                     epsilon[i])
    }
  }
  
  return(y_hat)
  
}
