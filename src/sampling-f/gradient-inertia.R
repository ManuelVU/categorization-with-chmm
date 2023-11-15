################################################################################
# Function for the partial derivatives of the full joint posterior distribution 
# of the stickiness parameters in the CHMM model.
# The function returns a vector with the value of the partial derivative of the 
# full joint conditional posterior distribution of f(alpha) = log(alpha) 
# and f(beta) = log(beta) as a two dimensional vector in the order 
# (f(alpha), f(beta))
################################################################################

# Function takes 8 arguments
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

gradient_inertia <- function(states,
                             alpha_tilde,
                             beta_tilde, 
                             alpha_prior,
                             beta_prior,
                             similarity, 
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
               dplyr::lead(states[s, ])[-total_trials] == 0) +
              (states[s, -total_trials] == 0 & 
                 dplyr::lead(states[s, ])[-total_trials] == 1),
            -(states[s, -total_trials] == 0 & 
                dplyr::lead(states[s, ])[-total_trials] == 1))
    
    transition_id_beta <- 
      cbind((states[s, -total_trials] == 1 & 
               dplyr::lead(states[s, ])[-total_trials] == 0) +
              (states[s, -total_trials] == 1 & 
                 dplyr::lead(states[s, ])[-total_trials] == 1),
            -(states[s, -total_trials] == 1 & 
               dplyr::lead(states[s, ])[-total_trials] == 0))
    
    exponential_alpha <- c()
    
    exponential_beta <- c()
    
    for (t in 2:total_trials) {
      
      relative_sim_others <- 
        state_similarity(states_vec = states_rest[, (t - 1)],
                         similarity_mat = similarity_to_others,
                         method = "average")
      
      exponential_alpha[(t - 1)] <- exp(alpha_tilde + relative_sim_others) /
        (exp(relative_sim_others) + exp(exp(e_alpha_tilde)))
      
      exponential_beta[(t - 1)] <- exp(beta_tilde - relative_sim_others) /
        (exp(-relative_sim_others) + exp(e_beta_tilde))
    }
    
    gradient_alpha <- 
      sum(transition_id_alpha *
            cbind(exponential_alpha,
                  rep(x = e_alpha_tilde, 
                      times = total_trials - 1))) + 
      alpha_prior[1] -
      alpha_prior[2] * e_alpha_tilde
    
    gradient_beta <- 
      sum(transition_id_beta *
            cbind(exponential_beta,
                  rep(x = e_beta_tilde, 
                      times = total_trials - 1))) + 
      beta_prior[1] -
      beta_prior[2] * e_beta_tilde
    
    gradient <- gradient + c(gradient_alpha, gradient_beta)
    
  }
  return(gradient)
}
