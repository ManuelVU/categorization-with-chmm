# Function that calculates the gradient of the log joint posterior of the 
# inertia parameters in the model.

gradient_inertia <- function(states,
                             alpha_tilde,
                             kappa_tilde,
                             alpha_prior,
                             kappa_prior,
                             similarity, 
                             total_trials,
                             n_stimulus,
                             state_similarity_function){
  
  gradient <- c(0, 0)
  
  e_alpha_tilde <- exp(alpha_tilde)
  e_kappa_tilde <- exp(kappa_tilde)
  
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
    
    transition_id_kappa <- cbind(-transition_id_alpha[, 1],
                                 -transition_id_alpha[, 2],
                                 transition_id_alpha[, 3],
                                 transition_id_alpha[, 4])
    
    exponential_alpha <- matrix(data = NA, nrow = total_trials - 1, ncol = 4)
    
    exponential_kappa <- matrix(data = NA, nrow = total_trials - 1, ncol = 4)
    
    for (t in 2:total_trials) {
      
      relative_sim_others <- 
        state_similarity(states_vec = states_rest[, (t - 1)],
                         similarity_mat = similarity_to_others,
                         method = state_similarity_function)
      
      logit_k1 <- logit(x = e_alpha_tilde -
                             e_kappa_tilde * relative_sim_others)
      
      logit_k2 <- logit(x = e_alpha_tilde +
                          e_kappa_tilde * relative_sim_others)
      
      exponential_alpha[(t - 1), 1:4] <- c(1 - logit_k1, logit_k1,
                                           logit_k2, 1 - logit_k2)
      
      exponential_kappa[(t - 1), 1:4] <- c((1 - logit_k1) * relative_sim_others,
                                           logit_k1 * relative_sim_others,
                                           logit_k2 * relative_sim_others,
                                           (1 - logit_k2) * relative_sim_others)
      
    }
    
    gradient_alpha <- 
      e_alpha_tilde * sum(transition_id_alpha * exponential_alpha) + 
      alpha_prior[1] -
      alpha_prior[2] * e_alpha_tilde
    
    gradient_kappa <- 
      e_kappa_tilde * sum(transition_id_kappa * exponential_kappa) + 
      kappa_prior[1] -
      kappa_prior[2] * e_kappa_tilde
    
    gradient <- gradient + c(gradient_alpha, gradient_kappa)
    
  }
  
  return(gradient)
  
}
