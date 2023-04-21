# Function that applies the forward filter backward sample algorithm to a 
# set of chains. 

# NOTE: unobserved_states should be organized with trial number as row and 
# stimulus number in the columns.

forward_backward <- function(update_stimulus_id, unobserved_states, responses,
                             similarity,
                             response_error, initial_probability, 
                             inertia_category_a, inertia_category_b){
  
  if (length(x = responses) != dim(unobserved_states)[1]){
    stop("Number of responses is larger than number of trials")
  }
  
  n_states <- length(x = unique(x = unobserved_states[, update_stimulus_id]))
  
  n_stimulus_rest <- dim()
  
  total_trials <- length(x = responses)
  
  conditional_predictive <- matrix(data = NA, nrow = total_trials,
                                   ncol = n_states)
  
  conditional_filtered <- matrix(data = NA, nrow = total_trials,
                                 ncol = n_states)
  
  conditional_filtered_rest <- matrix(data = NA, nrow = total_trials,
                                      ncol = n_states)
  
  if (is.na(responses[1])) {
    response_state <- c(1, 1)
  } 
  else {
    response_state <- c(response_error^responses[1] * 
                          (1 - response_error)^(1 - responses[1]),
                        response_error^(1 - responses[1]) * 
                          (1-response_error)^responses[1])
  }
  
  conditional_predictive[1, ] <- c(1 - initial_probability,
                                   initial_probability)
  
  conditional_filtered_rest <- 1 # This should be the outcome of a new function
                                 # that will be implemented in R for now and 
                                 # moved to c++ hopefully
  
  print(response_state)
  print(n_states)
  print(total_trials)
  print(conditional_predictive)
}



# Test function

forward_backward(responses = rbinom(n = 5, size = 1, prob = 0.5), 
                 update_stimulus = 1, 
                 unobserved_states = 
                   matrix(rbinom(n = 10, size = 1, prob = 0.5),
                          ncol = 2, nrow = 5),
                 response_error = 0.1,
                 initial_probability = 0.2)
