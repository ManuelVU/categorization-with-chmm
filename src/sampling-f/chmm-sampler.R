# CHMM of categorization sampling algorithm

chmm_sampling <- function(data_chmm,
                          n_iterations, n_burn, n_cores,
                          parameters_initial_values) {
  
  # Separate data
  responses <- data_chmm$response
  trials_participant <- data_chmm$participant_t
  features <- data_chmm$stimulus_features
  
  # Obtain similarity matrix from stimulus features
  stimulus_distinctive <- distinctive_ln(stimulus_features = features)
  stimulus_distance <- featural_distance(distinctive_features = 
                                           stimulus_distinctive)
  stimulus_similarity <- similarity_ij(decay_rate = 1, decay_function = 1, 
                                       dissimilarity = stimulus_distance)

  # Start sample vectors for model parameters add initial value to position 1
  sample_gamma <- append(x = c(), values = parameters_initial_values$gamma)
  sample_alpha <- append(x = c(), values = parameters_initial_values$alpha)
  sample_beta <- append(x = c(), values = parameters_initial_values$beta)
  
  # Add initial sample of states to initial values
  parameters_initial_values$states <- initial_states(
    n_chains = dim(responses)[1], 
    n_trials = trials_participant,
    n_participants = dim(responses)[3], 
    similarity = stimulus_similarity,
    initial_state_probability = sample_gamma,
    inertia_category_a = sample_alpha,
    inertia_category_b = sample_beta)
  
  # Start states array to store samples of states
  sample_states <- array(data = NA, dim = c(dim(responses), n_iterations))
  
  # Initialize states at their initial values
  sample_states[, , , 1] <- parameters_initial_values$states
  
  return(parameters_initial_values) 
}


# Test

this <- transform_data_chmm(
  directory_data = "data/csv-files/lee-navarro-2002-type4.csv",
  directory_features = "data/stimulus-features/lee-navarro-features.csv")



chmm_sampling(data_chmm = this,
              n_iterations = 100,
              n_burn = 10,
              parameters_initial_values =
                list("gamma" = 0.5,
                     "alpha" = rgamma(n = dim(this$response)[3],
                                      shape = 2, rate = 1),
                     "beta" = rgamma(n = dim(this$response)[3],
                                     shape = 2, rate = 1)))
