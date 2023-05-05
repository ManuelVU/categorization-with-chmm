# Function that simulates participants behavior according to the CHMM for 
# Lee and Navarro's 2002 condition 4 and Lewandowsky's 2011 condition 6.

simulate_chmm <- function(n_participants, n_trials, experiment, n_stimulus,
                          stimulus_trial = n_stimulus, 
                          gamma, epsilon_p, alpha_p, beta_p) {
  
  if (experiment == "lee_navarro") {
    features <- readr::read_csv(
      file = "data/stimulus-features/lee-navarro-features.csv")
  }
  else {
    features <- readr::read_csv(
      file = "data/stimulus-features/lewandowsky-features.csv")
  }
  
  stimulus_distinctive <- distinctive_ln(stimulus_features = features)
  
  stimulus_distance <- 
    featural_distance(distinctive_features = stimulus_distinctive)
  
  stimulus_similarity <- similarity_ij(decay_rate = 1, decay_function = 1, 
                                       dissimilarity = stimulus_distance)
  
  states <- array(data = NA, dim = c(n_stimulus, n_trials, n_participants))
  
  responses <- array(data = NA, dim = c(n_stimulus, n_trials, n_participants))
  
  states[, 1, ] <- matrix(data = rbinom(n = n_participants * n_stimulus,
                                        size = 1, prob = gamma), 
                          nrow = n_stimulus,
                          ncol = n_participants)
  
  responses[, 1, ] <- matrix(data = states[, 1, ] * 
                               matrix(data = 
                                        rbinom(n = n_participants * n_stimulus,
                                               size = 1, 
                                               prob = rep(x = (1 - epsilon_p),
                                                          each = n_stimulus)), 
                                      nrow = n_stimulus,
                                      ncol = n_participants) +
                               (1 - states[, 1, ]) * 
                               matrix(data = 
                                        rbinom(n = n_participants * n_stimulus,
                                               size = 1, 
                                               prob = rep(x = epsilon_p,
                                                          each = n_stimulus)), 
                                      nrow = n_stimulus,
                                      ncol = n_participants),
                             nrow = n_stimulus, 
                             ncol = n_participants)
  
  for (pp in 1:n_participants) {
    
    for (tt in 2:n_trials) {
      
      for (cc in 1:n_stimulus){
        
        similarity_to_others <- (stimulus_similarity[cc, ])[-cc]
        
        relative_sim_others <- similarity_to_others %*%
          (2 * states[-cc, (tt - 1), pp] - 1)
        
        prob_stay_a <- logit(x = alpha_p[pp] - relative_sim_others)
        
        prob_stay_b <- logit(x = beta_p[pp] + relative_sim_others)
        
        states[cc, tt, pp] <- ifelse(test = states[cc, (tt - 1), pp] == 1, 
                                     yes = rbinom(n = 1, size = 1, 
                                                  prob = prob_stay_b),
                                     no = rbinom(n = 1, size = 1, 
                                                 prob = (1 - prob_stay_a)))
        
        responses[cc, tt, pp] <- states[cc, tt, pp] *
          rbinom(n = 1, size = 1, prob = (1 - epsilon_p[pp])) +
          (1 - states[cc, tt, pp]) * 
          rbinom(n = 1, size = 1, prob = epsilon_p[pp])
      }
    }
  }
  output <- list("hidden_states" = states,  
                 "response" = responses,
                 "simulation_parameters" = 
                   list("initial_probability" = gamma,
                        "probability_error" = epsilon_p,
                        "inertia_categoy_a" = alpha_p,
                        "inertia_categoy_b" = beta_p))
  return(output)
}



# test
simulate_chmm(n_participants = 3, n_trials = 5, n_stimulus = 9,
              experiment = "lee_navarro", gamma = 0.5, 
              epsilon_p = c(0.01, 0.01, 0.01),
              alpha_p = c(0.1, 2, 5),
              beta_p = c(0.1, 2, 5))

