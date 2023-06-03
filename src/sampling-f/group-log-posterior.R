# Group level log posterior distribution for hamiltonian algorithm

log_posterior_group <- function(states, total_trials,
                                alpha_tilde, beta_tilde,
                                alpha_prior, beta_prior,
                                similarity, n_stimulus) {
  
  n_participants <- dim(states)[3]
  
  l_posterior <- 0

  for (p in 1:n_participants) {
    
    states_p <- states[, 1:total_trials[p], p]
    
    for (s in 1:n_stimulus) {
      
      similarity_to_others <- (similarity[s, ])[s]
      
      states_rest <- states_p[-s, ]
      
      transition_id <- cbind(states_p[s, -total_trials[p]] == 0 & 
                               dplyr::lead(states_p[s, ])[-total_trials[p]] == 0,
                             states_p[s, -total_trials[p]] == 0 & 
                               dplyr::lead(states_p[s, ])[-total_trials[p]] == 1,
                             states_p[s, -total_trials[p]] == 1 & 
                               dplyr::lead(states_p[s, ])[-total_trials[p]] == 0,
                             states_p[s, -total_trials[p]] == 1 & 
                               dplyr::lead(states_p[s, ])[-total_trials[p]] == 1)
      
      for (t in 2:total_trials[p]) {
        
        relative_sim_others <- 
          state_similarity(states_vec = states_rest[, (t - 1)],
                           similarity_mat = similarity_to_others,
                           method = "average")
        
        prob_stay_a <- logit(x = exp(alpha_tilde) - relative_sim_others)
        prob_stay_a <- max(0.0001, min(0.9999, prob_stay_a))
        
        prob_stay_b <- logit(x = exp(beta_tilde) + relative_sim_others)
        prob_stay_b <- max(0.0001, min(0.9999, prob_stay_b))
        
        log_transitions <- log(c(prob_stay_a, 1 - prob_stay_a, 
                                 1 - prob_stay_b, prob_stay_b))
        
        l_posterior <- l_posterior + transition_id[(t - 1),] %*% log_transitions
        
      }
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

# a <- readr::read_csv(file = "data/stimulus-features/lee-navarro-features.csv")
# b <- distinctive_ln(stimulus_features = a)
# d <- featural_distance(distinctive_features = b)
# k <- similarity_ij(decay_rate = 1, decay_function = 1, dissimilarity = d)
# 
# log_posterior_group(alpha_tilde = 2, beta_tilde = 1,
#                        states = st,
#                        total_trials = rep(5,10), n_stimulus = 9, similarity = k,
#                        alpha_prior = c(2, 1), beta_prior = c(2, 1))
# 
# 
# log_posterior_group(alpha_tilde = 0.1, beta_tilde = 0.1,
#                     states = st,
#                     total_trials = rep(5,10), n_stimulus = 9, similarity = k,
#                     alpha_prior = c(2, 1), beta_prior = c(2, 1))
