# Function that calculates the gradient of the log joint posterior of the 
# inertia parameters in the model.

gradient_inertia <- function(states,
                             alpha_tilde,
                             alpha_prior,
                             similarity,
                             total_trials,
                             n_stimulus,
                             state_similarity_function){
  
  gradient <- c(0)
  
  e_alpha_tilde <- exp(alpha_tilde)
  
  for (s in 1:n_stimulus) {
    
    similarity_to_others <- (similarity[s, ])[-s]
    
    states_rest <- states[-s, ]
    
    transition_id_alpha <- 
      cbind((states[s, -total_trials] == 0 & 
               dplyr::lead(states[s, ])[-total_trials] == 0),
            -(states[s, -total_trials] == 0 & 
                dplyr::lead(states[s, ])[-total_trials] == 1),
            -(states[s, -total_trials] == 1 & 
                dplyr::lead(states[s, ])[-total_trials] == 0),
            (states[s, -total_trials] == 1 & 
               dplyr::lead(states[s, ])[-total_trials] == 1))
    
    exponential_alpha <- matrix(data = NA, nrow = total_trials - 1, ncol = 4)
    
    for (t in 2:total_trials) {
      
      relative_sim_others <- 
        state_similarity(states_vec = states_rest[, (t - 1)],
                         similarity_mat = similarity_to_others,
                         method = state_similarity_function)
      
      logit_k1 <- logit(x = e_alpha_tilde)
      
      logit_k2 <- logit(x = e_alpha_tilde)
      
      exponential_alpha[(t - 1), 1:4] <- c(1 - logit_k1, logit_k1,
                                           logit_k2, 1 - logit_k2)
      
    }
    
    gradient_alpha <- 
      e_alpha_tilde * sum(transition_id_alpha * exponential_alpha) + 
      alpha_prior[1] -
      alpha_prior[2] * e_alpha_tilde
    
    gradient <- gradient + c(gradient_alpha)
    
  }
  
  return(gradient)
  
}
