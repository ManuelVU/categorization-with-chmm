################################################################################
# Forward Filter Backward Sample algorithm.
# The function returns a list with a sample of the conditional posterior 
# distribution of the unobserved states and the summed log-likelihood for the 
# sequence
################################################################################

# This function takes 10 arguments
#   1:  update_stimulus_id, id of the stimulus to be updated.
#   2:  unobserved_states, current sample of the hidden states for all stimuli.
#   3:  responses, vector of responses across trials. 
#   4:  similarity, a symmetric similarity matrix. 
#   5:  n_states, number of possible states a chain can take.
#   6:  total_trials, total number of trials for a given participant.
#   7:  response_error, value for the trembling-hand parameter.
#   8:  initial_probability, initial probability of states (vector of size N -1)
#   9:  inertia_category_a, stickiness parameter in favor of category 0.
#   10: inertia_category_b, stickiness parameter in favor of category 1. 

forward_backward <- function(update_stimulus_id,
                             unobserved_states,
                             responses,
                             similarity,
                             n_states,
                             total_trials,
                             response_error,
                             initial_probability,
                             inertia_category_a,
                             inertia_category_b){
  
  states_rest <- unobserved_states[-update_stimulus_id, ]
  
  diag(similarity) <- 0
  
  similarity_to_others <- (similarity[update_stimulus_id, ])[-update_stimulus_id]
  
  conditional_predictive <- matrix(data = NA, nrow = n_states,
                                   ncol = total_trials)
  
  conditional_filtered <- matrix(data = NA, nrow = n_states,
                                 ncol = total_trials)
  
  conditional_filtered_rest <- matrix(data = NA, nrow = n_states,
                                      ncol = total_trials)
  
  likelihood <- c()
  
  # Forward Filter
  
  if (is.na(responses[1])) {
    response_state <- c(1, 1)
  } 
  else {
    response_state <- c(response_error^responses[1] * 
                          (1 - response_error)^(1 - responses[1]),
                        response_error^(1 - responses[1]) * 
                          (1-response_error)^responses[1])
  }
  
  conditional_predictive[, 1] <- c(1 - initial_probability,
                                   initial_probability)
  
  conditional_filtered_rest[, 1] <- 
    transition_others(state_now = states_rest[, 1],
                      state_after = states_rest[, 2],
                      similarity = similarity, 
                      current_id = update_stimulus_id,
                      alpha = inertia_category_a, 
                      beta = inertia_category_b)
  
  conditional_filtered[, 1] <- (response_state * 
    conditional_predictive[, 1] * 
    conditional_filtered_rest[, 1]) / 
    sum((response_state * 
           conditional_predictive[, 1] * 
           conditional_filtered_rest[, 1]))
  
  for (t in 2:total_trials) {
    relative_sim_others <- 
      state_similarity(states_vec = states_rest[, (t - 1)],
                       similarity_mat = similarity_to_others,
                       method = "average")
    
    prob_stay_a <- logit(x = inertia_category_a - relative_sim_others)
    prob_stay_b <- logit(x = inertia_category_b + relative_sim_others)
    
    conditional_predictive[, t] <- c(
      prob_stay_a * conditional_filtered[1, (t - 1)] +
      (1 - prob_stay_b) * conditional_filtered[2, (t - 1)],
      (1 - prob_stay_a) * conditional_filtered[1, (t - 1)] +
      prob_stay_b * conditional_filtered[2, (t - 1)])
    
    if (is.na(responses[t])) {
      response_state <- c(1, 1)
    } 
    else {
      response_state <- c(response_error^responses[t] * 
                            (1 - response_error)^(1 - responses[t]),
                          response_error^(1 - responses[t]) * 
                            (1-response_error)^responses[t])
    }
    
    if (t == total_trials) {
      conditional_filtered_rest[, t] <- c(1, 1)
    }
    else {
      conditional_filtered_rest[, t] <- 
        transition_others(state_now = states_rest[, t],
                          state_after = states_rest[, (t + 1)],
                          similarity = similarity, 
                          current_id = update_stimulus_id,
                          alpha = inertia_category_a, 
                          beta = inertia_category_b)
    }
    
    conditional_filtered[, t] <-
      (response_state * 
      conditional_predictive[, t] * 
      conditional_filtered_rest[, t]) / 
      sum((response_state *
             conditional_predictive[, t] *
             conditional_filtered_rest[, t]))
  }
  
  # Backward Sample
  
  prob_category_b <- conditional_filtered[2, total_trials]
  
  unobserved_states[update_stimulus_id, total_trials] <- 
    rbinom(n = 1, size = 1, prob = prob_category_b)
  
  likelihood[total_trials] <- 
    ifelse(test = unobserved_states[update_stimulus_id, total_trials] == 1,
           yes = prob_category_b, no = 1 - prob_category_b)
  
  for (t in (total_trials - 1):1) {
    
    relative_sim_others <- 
      state_similarity(states_vec = states_rest[, t],
                       similarity_mat = similarity_to_others, 
                       method = "average")
    
    prob_stay_b <- logit(x = inertia_category_b + relative_sim_others)
    
    if (unobserved_states[update_stimulus_id, (t + 1)] == 1) {
      
      prob_category_b <- (prob_stay_b * conditional_filtered[2, t]) /
        conditional_predictive[2, (t + 1)]
      
      unobserved_states[update_stimulus_id, t] <- 
        rbinom(n = 1, size = 1, prob = prob_category_b)
      
      likelihood[t] <- 
        ifelse(test = unobserved_states[update_stimulus_id, t] == 1,
               yes = prob_category_b, no = 1 - prob_category_b)
    }
    else {
      
      prob_category_b <- ((1 - prob_stay_b) * conditional_filtered[2, t]) /
        conditional_predictive[1, (t + 1)]
      
      unobserved_states[update_stimulus_id, t] <- 
        rbinom(n = 1, size = 1, prob = prob_category_b)
      
      likelihood[t] <- 
        ifelse(test = unobserved_states[update_stimulus_id, t] == 1,
               yes = prob_category_b, no = 1 - prob_category_b)
    }
  }
  
  return(list(unobserved_states[update_stimulus_id, ], sum(log(likelihood))))
  
}
