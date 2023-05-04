# Function used to generate the initial value of hidden states of all chains 
# and participants.

initial_states <- function(n_chains, n_trials, n_participants,
                           similarity, initial_state_probability,
                           inertia_category_a, inertia_category_b) {

  states <- array(data = NA, dim = c(n_chains, n_trials, n_participants))
  
  for (p in 1:n_participants){
   
    states[, 1, p] <- rbinom(n = n_chains, size = 1, 
                             prob = initial_state_probability)
   
    for (t in 2:n_trials) {
      for (cc in 1:n_chains) {
        
        similarity_to_others <- (similarity[cc, ])[-cc]
        
        relative_sim_others <- similarity_to_others %*%
          (2 * states[-cc, (t - 1), p] - 1)
        
        prob_stay_a <- logit(x = inertia_category_a[p] - relative_sim_others)
        prob_stay_b <- logit(x = inertia_category_b[p] + relative_sim_others)
        
        states[cc, t, p] <- ifelse(test = states[cc, (t - 1), p] == 1, 
                                   yes = rbinom(n = 1, size = 1, 
                                                prob = prob_stay_b),
                                   no = rbinom(n = 1, size = 1, 
                                               prob = (1 - prob_stay_a)))
      }
    }
  }
  return(states)
}

# test
# a <- readr::read_csv(file = "data/stimulus-features/lee-navarro-features.csv")
# b <- distinctive_ln(stimulus_features = a)
# d <- featural_distance(distinctive_features = b)
# sm <- similarity_ij(decay_rate = 1, decay_function = 1, dissimilarity = d)
# 
# initial_states(n_chains = 9, n_trials = 20, n_participants = 10, 
#                similarity = sm, initial_state_probability = 0.5, 
#                inertia_category_a = rep(x = 0.1, times = 20), 
#                inertia_category_b = rep(x = 0.1, times = 20))
