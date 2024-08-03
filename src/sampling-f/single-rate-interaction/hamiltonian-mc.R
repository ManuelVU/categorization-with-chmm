# Function for updating the logarithm of the inertia parameters using the 
# Hamiltonian Monte Carlo algorithm

hamiltonian_mc <- function(states,
                           alpha_tilde,
                           kappa_tilde,
                           alpha_prior,
                           kappa_prior,
                           similarity,
                           leap_size,
                           leap,
                           method_state_sim) {
  
  total_trials <- dim(states)[2]
  
  n_stimulus <- dim(states)[1]
  
  current_position <- c(alpha_tilde, kappa_tilde)
  
  position <- current_position
  
  momentum <- rnorm(n = length(current_position), mean = 0, sd = 1)
  
  momentum_current <- momentum
  
  momentum <- momentum - leap_size * 
    gradient_inertia(states = states,
                     alpha_tilde = alpha_tilde,
                     kappa_tilde = kappa_tilde,
                     alpha_prior = alpha_prior,
                     kappa_prior = kappa_prior,
                     similarity = similarity,
                     total_trials = total_trials,
                     n_stimulus = n_stimulus,
                     state_similarity_function = method_state_sim) / 2
  
  leap <- ceiling(runif(n = 1) * leap) - 1
  
  for (k in 1:leap) {
    
    position <- position + leap_size * momentum
    
    if (k != leap) {
      momentum <- momentum - leap_size * 
        gradient_inertia(states = states,
                         alpha_tilde = alpha_tilde,
                         kappa_tilde = kappa_tilde,
                         alpha_prior = alpha_prior,
                         kappa_prior = kappa_prior,
                         similarity = similarity,
                         total_trials = total_trials,
                         n_stimulus = n_stimulus,
                         state_similarity_function = method_state_sim)
    }
  }
  
  momentum <- momentum - leap_size * 
    gradient_inertia(states = states,
                     alpha_tilde = alpha_tilde,
                     kappa_tilde = kappa_tilde,
                     alpha_prior = alpha_prior,
                     kappa_prior = kappa_prior,
                     similarity = similarity,
                     total_trials = total_trials,
                     n_stimulus = n_stimulus,
                     state_similarity_function = method_state_sim) / 2
  
  momentum <- -momentum
  
  kinetic_current <- sum(momentum_current^2) / 2
  
  kinetic_proposed <- sum(momentum^2) / 2
  
  potential_current <- kinetic_current - 
    log_posterior(states = states, 
                  alpha_tilde = current_position[1],
                  kappa_tilde = current_position[2],
                  alpha_prior = alpha_prior,
                  kappa_prior = kappa_prior,
                  similarity = similarity,
                  total_trials = total_trials,
                  n_stimulus = n_stimulus,
                  state_similarity_function = method_state_sim) 
  
  potential_proposed <- kinetic_proposed -
    log_posterior(states = states, 
                  alpha_tilde = position[1],
                  kappa_tilde = position[2],
                  alpha_prior = alpha_prior,
                  kappa_prior = kappa_prior,
                  similarity = similarity,
                  total_trials = total_trials,
                  n_stimulus = n_stimulus,
                  state_similarity_function = method_state_sim)
  
  
  acceptance_probability <- min(1, exp(potential_current - potential_proposed))

  ifelse(test = is.finite(exp(position[1])) & is.finite(exp(position[2])),
         yes = ifelse(test = runif(n = 1) < acceptance_probability, 
                      yes = return(list(position, 1)),
                      no = return(list(current_position, 0))),
         no = return(list(current_position, 0)))
}
