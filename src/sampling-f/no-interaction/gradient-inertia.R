################################################################################
## Function that calculates the gradient of the log joint posterior of the    ##
## inertia and generalization parameter in the CHMM model                     ##
################################################################################

gradient_inertia <- function(states,
                             alpha_tilde,
                             beta_tilde,
                             similarity,
                             alpha_prior,
                             beta_prior,
                             total_trials,
                             n_stimulus){
  
  gradient <- c(0, 0)
  
  e_alpha_tilde <- exp(alpha_tilde)
  e_beta_tilde <- exp(beta_tilde)
  
  for (s in 1:n_stimulus) {
    
    similarity_to_others <- (similarity[s, ])[-s]
    
    states_rest <- states[-s, ]
    
    transition_id_alpha <- 
      cbind((states[s, -total_trials] == 0 & 
               dplyr::lead(states[s, ])[-total_trials] == 0),
            -(states[s, -total_trials] == 0 & 
                dplyr::lead(states[s, ])[-total_trials] == 1))
    
    transition_id_beta <- 
      cbind(-(states[s, -total_trials] == 1 & 
                dplyr::lead(states[s, ])[-total_trials] == 0),
            (states[s, -total_trials] == 1 & 
               dplyr::lead(states[s, ])[-total_trials] == 1))
    
    exponential_alpha <- matrix(data = NA, nrow = total_trials - 1, ncol = 2)
    
    exponential_beta <- matrix(data = NA, nrow = total_trials - 1, ncol = 2)
    
    for (t in 2:total_trials) {
      
      relative_sim_others <- 
        state_similarity(states_vec = states_rest[, (t - 1)],
                         similarity_mat = similarity_to_others)
      
      logit_alpha <- logit(x = e_alpha_tilde)
      
      exponential_alpha[(t - 1), 1] <- 1 - logit_alpha
      exponential_alpha[(t - 1), 2] <- logit_alpha
      
      logit_beta <- logit(x = e_beta_tilde)
      
      exponential_beta[(t - 1), 1] <- logit_beta
      exponential_beta[(t - 1), 2] <- 1 - logit_beta
      
    }
    
    gradient_alpha <-
      e_alpha_tilde * sum(transition_id_alpha * exponential_alpha)
    
    gradient_beta <-
      e_beta_tilde * sum(transition_id_beta * exponential_beta)
    
    gradient <- gradient + c(gradient_alpha, gradient_beta)
    
  }
  
  gradient <- gradient + c(alpha_prior[1] - alpha_prior[2] * e_alpha_tilde,
                           beta_prior[1] - beta_prior[2] * e_beta_tilde)
  
  return(gradient)
  
}
