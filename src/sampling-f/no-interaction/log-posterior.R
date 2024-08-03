################################################################################
# Function calculates the logarithm of the full joint conditional posterior 
# distribution of the logarithm of the stickiness parameters in the CHMM model 
# f(alpha) = log(alpha) and f(beta) = log(beta)
# Function returns a single value with the evaluation of the logarithm of the 
# full joint conditional posterior distribution of the parameters f(alpha) and 
# f(beta) evaluated at the values alpha_tilde and beta_tilde 
################################################################################

# This function takes 8 arguments
#   1: states, current sample of the hidden states for a given participant p.
#   2: alpha_tilde, value of the logarithm of the stickiness parameter alpha.
#   3: beta_tilde, value of the logarithm of the stickiness parameter beta.
#   4: alpha_prior, values of the parameters of the gamma prior distribution of 
#      the stickiness parameter alpha.
#   5: beta_prior, values of the parameters of the gamma prior distribution of 
#      the stickiness parameter beta.
#   6: similarity, symmetric between stimuli similarity matrix.
#   7: total_trials, total number of trials of participant p.
#   8: n_stimulus, total number of stimuli in the task.

log_posterior <- function(states,
                          alpha_tilde,
                          beta_tilde,
                          alpha_prior,
                          beta_prior,
                          similarity,
                          total_trials,
                          n_stimulus){
  
  l_posterior <- 0
  
  for (s in 1:n_stimulus) {
    
    similarity_to_others <- (similarity[s, ])[s]
    
    states_rest <- states[-s, ]
    
    transition_id <- cbind(states[s, -total_trials] == 0 & 
                             dplyr::lead(states[s, ])[-total_trials] == 0,
                           states[s, -total_trials] == 0 & 
                             dplyr::lead(states[s, ])[-total_trials] == 1,
                           states[s, -total_trials] == 1 & 
                             dplyr::lead(states[s, ])[-total_trials] == 0,
                           states[s, -total_trials] == 1 & 
                             dplyr::lead(states[s, ])[-total_trials] == 1)
    
    for (t in 2:total_trials) {
      
      relative_sim_others <- 
        state_similarity(states_vec = states_rest[, (t - 1)],
                         similarity_mat = similarity_to_others,
                         method = "average")
      
      prob_stay_a <- logit(x = exp(alpha_tilde))
      prob_stay_a <- max(0.0001, min(0.9999, prob_stay_a))
      
      prob_stay_b <- logit(x = exp(beta_tilde))
      prob_stay_b <- max(0.0001, min(0.9999, prob_stay_b))
      
      log_transitions <- log(c(prob_stay_a, 1 - prob_stay_a, 
                               1 - prob_stay_b, prob_stay_b))
      
      l_posterior <- l_posterior + transition_id[(t - 1),] %*% log_transitions
      
    }
  }
  
  l_posterior <- l_posterior + 
    dgamma(x = exp(alpha_tilde), shape = alpha_prior[1], rate = alpha_prior[2], 
           log = TRUE) +
    alpha_tilde +
    dgamma(x = exp(beta_tilde), shape = beta_prior[1], rate = beta_prior[2],
           log = TRUE) +
    beta_tilde
  
  return(l_posterior)

}
