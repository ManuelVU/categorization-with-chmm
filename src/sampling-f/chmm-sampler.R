# CHMM of categorization sampling algorithm

chmm_sampling <- function(data_chmm,
                          n_iterations, n_burn, n_cores,
                          parameters_initial_values) {
  
  # Load library for parallel computing and register cores
  library(doParallel)
  library(foreach)
  if(n_cores > parallel::detectCores()){
    message("Number of cores selected is greater than available number.")
    
    message(paste(c("Setting number of cores to: ", parallel::detectCores()),
                  collapse = ""))
    
    n_cores <- parallel::detectCores()
  }
  
  doParallel::registerDoParallel(n_cores)
  
  # Separate data
  responses <- data_chmm$response
  trials_participant <- data_chmm$participant_t
  features <- data_chmm$stimulus_features
  participants <- dim(responses)[3]
  stimulus <- dim(responses)[1]
  
  # Obtain similarity matrix from stimulus features
  stimulus_distinctive <- distinctive_ln(stimulus_features = features)
  stimulus_distance <- featural_distance(distinctive_features = 
                                           stimulus_distinctive)
  stimulus_similarity <- similarity_ij(decay_rate = 1, decay_function = 1, 
                                       dissimilarity = stimulus_distance)

  # Start sample vector for initial probability parameter and add initial 
  # value to position 1
  sample_gamma <- append(x = parameters_initial_values$gamma, 
                         values = rep(x = NA, times = n_iterations - 1))
  
  # Start sample matrix for individual model parameters and add initial value 
  # to row one
  sample_epsilon <- matrix(data = parameters_initial_values$epsilon, 
                           byrow = TRUE, 
                           ncol = participants, 
                           nrow = n_iterations)
  sample_alpha <- matrix(data = parameters_initial_values$alpha, 
                         byrow = TRUE, 
                         ncol = participants, 
                         nrow = n_iterations)
  sample_beta <- matrix(data = parameters_initial_values$beta, 
                        byrow = TRUE, 
                        ncol = participants, 
                        nrow = n_iterations)
  
  # Add initial sample of states to initial values
  parameters_initial_values$states <- initial_states(
    n_chains = stimulus, 
    n_trials = trials_participant,
    n_participants = participants, 
    similarity = stimulus_similarity,
    initial_state_probability = sample_gamma[1],
    inertia_category_a = sample_alpha[1,],
    inertia_category_b = sample_beta[1,])
  
  # Start states array to store samples of states
  sample_states <- array(data = NA, dim = c(dim(responses), n_iterations))
  
  # Initialize states at their initial values
  sample_states[, , , 1] <- parameters_initial_values$states
  
  # Initialize step size and number of steps for HMC
  step_size <- 0.03
  n_leaps <- 30
  
  # Start sampler
  for (sample in 2:n_iterations) {
    # st <- foreach(pp = 1:participants) %dopar% {
    for (pp in 1:participants) {
      forward_backward_all(states_current = 
                             sample_states[, 1:trials_participant[pp], pp, 
                                           (sample - 1)],
                           responses = responses[, 1:trials_participant[pp], pp],
                           similarity = stimulus_similarity,
                           n_states = 2, 
                           total_trials = trials_participant[pp],
                           total_chains = stimulus,
                           epsilon = sample_epsilon[(sample - 1), pp],
                           gamma = sample_gamma[(sample - 1)],
                           alpha = sample_alpha[(sample - 1), pp],
                           beta = sample_beta[(sample - 1), pp])
    }
    # }
    
  # Move current sample of states to sample array
    # for (pp in 1:participants) {
    #   sample_states[, , pp, sample] <- st[[pp]]
    # }
    
  }
  
  return(sample_states)
}


# Test
# 
# this <- transform_data_chmm(
#   directory_data = "data/csv-files/lee-navarro-2002-type4.csv",
#   directory_features = "data/stimulus-features/lee-navarro-features.csv")
# 
# 
# 
chmm_sampling(data_chmm = this,
              n_iterations = 3,
              n_burn = 10,
              n_cores = 4,
              parameters_initial_values =
                list("gamma" = 0.5,
                     "epsilon" = rbeta(n = dim(this$response)[3],
                                       shape1 = 6,
                                       shape2 = 60),
                     "alpha" = rgamma(n = dim(this$response)[3],
                                      shape = 2, rate = 1),
                     "beta" = rgamma(n = dim(this$response)[3],
                                     shape = 2, rate = 1)))

