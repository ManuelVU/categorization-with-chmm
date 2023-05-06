# CHMM of categorization sampling algorithm

chmm_sampling <- function(data_chmm, stimulus_features,
                          n_iterations, n_burn,
                          parameters_initial_values) {
  
  responses <- data_chmm$response
  
  trials_participant <- data_chmm$participant_t

  stimulus_features <- data_chmm$stimulus_features

  theta <- parameters_initial_values$parameters
  
  states <- array(data = NA, dim = c(dim(responses), n_iterations))
  
  states[, , , 1] <- parameters_initial_values$states
  
}
