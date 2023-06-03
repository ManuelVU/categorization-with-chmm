# Group level gradient function of the inertia parameters

gradient_inertia_group <- function(states, 
                             alpha_tilde, beta_tilde, 
                             alpha_prior, beta_prior, similarity, 
                             total_trials, n_stimulus){
  
  n_participants <- dim(states)[3]
  
  gradient <- c(0, 0)
  
  e_alpha_tilde <- exp(alpha_tilde)
  e_beta_tilde <- exp(beta_tilde)
  
  for (p in 1:n_participants) {
    
    states_p <- states[, 1:total_trials[p], p]
    
    for (s in 1:n_stimulus) {
      
      states_rest <- states_p[-s, ]
      
      similarity_to_others <- (similarity[s, ])[-s]
      
      transition_id_alpha <- 
        cbind((states_p[s, -total_trials[p]] == 0 & 
                 dplyr::lead(states_p[s, ])[-total_trials[p]] == 0) +
                (states_p[s, -total_trials[p]] == 0 & 
                   dplyr::lead(states_p[s, ])[-total_trials[p]] == 1),
              -(states_p[s, -total_trials[p]] == 0 & 
                  dplyr::lead(states_p[s, ])[-total_trials[p]] == 1))
      
      transition_id_beta <- 
        cbind((states_p[s, -total_trials[p]] == 1 & 
                 dplyr::lead(states_p[s, ])[-total_trials[p]] == 0) +
                (states_p[s, -total_trials[p]] == 1 & 
                   dplyr::lead(states_p[s, ])[-total_trials[p]] == 1),
              -(states_p[s, -total_trials[p]] == 1 & 
                  dplyr::lead(states_p[s, ])[-total_trials[p]] == 0))
      
      exponential_alpha <- c()
      
      exponential_beta <- c()
      
      for (t in 2:total_trials[p]) {
        
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
                        times = total_trials[p] - 1))) + 
        alpha_prior[1] -
        alpha_prior[2] * e_alpha_tilde
      
      gradient_beta <- 
        sum(transition_id_beta *
              cbind(exponential_beta,
                    rep(x = e_beta_tilde, 
                        times = total_trials[p] - 1))) + 
        beta_prior[1] -
        beta_prior[2] * e_beta_tilde
      
      gradient <- gradient + c(gradient_alpha, gradient_beta)
      
    }
  }
  
  return(gradient)
}

a <- readr::read_csv(file = "data/stimulus-features/lee-navarro-features.csv")
b <- distinctive_ln(stimulus_features = a)
d <- featural_distance(distinctive_features = b)
k <- similarity_ij(decay_rate = 1, decay_function = 1, dissimilarity = d)
# 
gradient_inertia_group(alpha_tilde = 2, beta_tilde = 1,
                       states = array(matrix(rbinom(n = 9 * 5, size = 1, prob = 0.5),
                                 ncol = 5, nrow = 9),dim = c(9,5,3)),
                        total_trials = rep(5,9), n_stimulus = 9, similarity = k,
                        alpha_prior = c(2, 1), beta_prior = c(2, 1))
