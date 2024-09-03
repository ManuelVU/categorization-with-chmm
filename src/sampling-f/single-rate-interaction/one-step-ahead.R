################################################################################
## This fuction is used to generate a one step ahead prediction fot both the  ##
## two and three parameter CHM model using the posterior samples for the      ##
## states and parameters as well as the experimental design for all           ##
## participants.                                                              ##
################################################################################

one_step_ahead <- function (data,
                            max_trials,
                            samples,
                            n_samples,
                            similarity,
                            interaction = TRUE) {

# check for a number of samples, if missing use all posterior samples
  n_samples <- ifelse(test = missing(n_samples),
                      yes = dim(samples$posterior_samples$hidden_states)[4],
                      no = n_samples)
# set sample ids
  if (n_samples < dim(samples$posterior_samples$hidden_states)[4]) {
    samples_id <- sample(x = seq(from = 1, to = n_samples, by = 1),
                          size = n_samples, replace = TRUE)
  }
  else {
    samples_id <- seq(seq(from = 1, to = n_samples, by = 1))
  }
  
# store the number of participants in the data
  n_participants <- length(unique(data$id))
  
# create object to store one-step-ahead predictions with a number of rows equal
# to the number of samples to take
  posterior <- array(data = NA, dim = c(n_participants, max_trials))
  
# start a for loop that runs over participants and trials
  for (n in 1:n_participants) {

# set participant id
    par_id <- unique(data$id)[n]
    
# subset participant data
    tmp_data <- subset(x = data, subset = id == par_id)
    
# number of trials for participant n
    n_trials <- dim(tmp_data)[1]
    
# create matrix to store agreement between simulated and observed responses
    agreement <- matrix(data = NA, nrow = n_samples, ncol = n_trials)
    
# start for loop for sample number
    for (s in 1:length(samples_id)) {

# set parameter values: stickiness, similarity weight, decision and h states
      alpha <- samples$posterior_samples$alpha[samples_id[s], n]
      if (interaction == TRUE) {
        kappa <- samples$posterior_samples$kappa[samples_id[s], n]  
      }
      else {
        kappa <- 0
      }
      epsilon <- samples$posterior_samples$epsilon[samples_id[s], n]
      hidden_states <- 
        samples$posterior_samples$hidden_states[, 1:n_trials, n, samples_id[s]]
      
# start for loop for participant trials
      for (t in 1:(n_trials - 1)) {
        
# find stimulus presented on trial t
        stimulus_id <- tmp_data$stimulus[t + 1]

# remove hidden state for the presented item
        hidden_others <- hidden_states[-stimulus_id, t]
        
# remove current stimulus from similarity matrix
        similarity_to_others <- (similarity[stimulus_id,])[-stimulus_id]
# calculate average similarity to others
        average_similarity <- state_similarity(states_vec = hidden_others,
                                               similarity_mat = 
                                                 similarity_to_others,
                                               method = "average")
        
# calculate probability of state one at t + 1
        probability_st1 <- ifelse(
          test = hidden_states[stimulus_id, t] == 1,
          yes = logit(x = alpha + kappa * average_similarity),
          no = 1 - logit(x = alpha - kappa * average_similarity))
        
# sample state t + 1
        sample_hidden_st <- rbinom(n = 1, size = 1, prob = probability_st1)
        
# calculate probability of response being 1 for trial t + 1
        probability_response <- ifelse(test = sample_hidden_st == 1,
                                       yes = 1 - epsilon,
                                       no = epsilon)
# sample response at t + 1
        sample_response <- rbinom(n = 1, size = 1, prob = probability_response)
        
# check for agreement between sample response and participant choice
        agreement[s, t + 1] <- as.numeric(
          x = sample_response == tmp_data$response[t + 1])
      }
    }
    
# estimate the mean agreement across posterior samples
    posterior[n, 2:n_trials] <- colMeans(x = agreement, na.rm = TRUE) [-1]
  }
# return as output the mean agreement between participants responses and samples
  return(posterior)
}
