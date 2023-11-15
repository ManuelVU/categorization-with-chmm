################################################################################
# Function applies the forward filter backward sample algorithm to all stimuli 
# for a single participant. 
# This function returns a sample of the hidden states of all stimuli at every 
# trial in matrix form. 
################################################################################

# This function takes 10 arguments
#   1:  states_current, current sample of the hidden states for a given 
#       participant.
#   2:  responses, participant's responses in matrix form.
#   3:  similarity, a symmetric between stimuli similarity matrix.
#   4:  n_states, the number of states in the model.
#   5:  total_trials, total number of trials of participant p.
#   6:  total_chains, total number of stimuli.
#   7:  epsilon, current value of the trembling hand parameter.
#   8:  gamma, current value of the initial probability parameter.
#   9:  alpha, current value of the stickiness parameter for category 0.
#   10: beta, current value of the stickiness parameter for category 1.

forward_backward_all <- function(states_current,
                                 responses,
                                 similarity, 
                                 n_states,
                                 total_trials,
                                 total_chains, 
                                 epsilon,
                                 gamma,
                                 alpha,
                                 beta){
    
  for(k in 1:total_chains){
    
    updated_chain <- forward_backward(update_stimulus_id = k,
                                      unobserved_states = states_current,
                                      responses = responses[k, ],
                                      similarity = similarity,
                                      n_states = n_states,
                                      total_trials = total_trials,
                                      response_error = epsilon,
                                      initial_probability = gamma,
                                      inertia_category_a = alpha,
                                      inertia_category_b = beta)
    
    states_current[k, ] <- updated_chain[[1]]
  }
  
  return(states_current)
  
}
