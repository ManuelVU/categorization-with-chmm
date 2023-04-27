# Function that calculates the logarthm of the joint full conditional for the 
# alpha and beta parameters in the model.

log_posterior <- function(alpha_tilde, beta_tilde, states, 
                          similarity, total_trials, n_stimulus){
  
  l_posterior <- 0
  
  for (s in 1:n_stimulus) {
    
    similarity_to_others <- similarity[-s, ]
    
    states_rest <- states[-s, ]
    
    transition_id <- cbind(states[s, -total_trials] == 0 & 
                             dplyr::lead(states[s, ])[-total_trials] == 0,
                           states[s, -total_trials] == 0 & 
                             dplyr::lead(states[s, ])[-total_trials] == 1,
                           states[s, -total_trials] == 1 & 
                             dplyr::lead(states[s, ])[-total_trials] == 0,
                           states[s, -total_trials] == 1 & 
                             dplyr::lead(states[s, ])[-total_trials] == 1)
    print(transition_id)
    
    for (t in 2:total_trials) {
      
      relative_sim_others <- sum(similarity_to_others * 
                                   (2 * states_rest[, (t - 1)] - 1))
      
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


# test

a <- readr::read_csv(file = "data/stimulus-features/lee-navarro-features.csv")
b <- distinctive_ln(stimulus_features = a)
d <- featural_distance(distinctive_features = b)
k <- similarity_ij(decay_rate = 1, decay_function = 1, dissimilarity = d)

log_posterior(alpha_tilde = 1, beta_tilde = 2, 
              states = matrix(rbinom(n = 9 * 5, size = 1, prob = 0.5),
                              ncol = 5, nrow = 9),
              total_trials = 5, n_stimulus = 9, similarity = k)
