################################################################################
# Hamiltonian Monte Carlo algorithm for the stickiness parameters.
# Function returns two elements on a list, first is a vector with the sampled 
# values of f(alpha) = log(alpha) and f(beta) = log(beta) respectively, and a 
# value of the potential energy of the sample that can be used in the next 
# iteration of the algorithm.
################################################################################

# Function takes 8 arguments
#   1: states, current sample of the hidden states for a given participant p.
#   2: alpha_tilde, value of the logarithm of the stickiness parameter alpha.
#   3: beta_tilde, value of the logarithm of the stickiness parameter beta.
#   4: alpha_prior, values of the parameters of the gamma prior distribution of 
#      the stickiness parameter alpha.
#   5: beta_prior, values of the parameters of the gamma prior distribution of 
#      the stickiness parameter beta.
#   6: similarity, symmetric between stimuli similarity matrix.
#   7: leap_size, step size to be used between computations of the momentum 
#      variable.
#   8: leap, number of iterations for the calculation of momentum.

hamiltonian_mc <- function(states,
                           alpha_tilde,
                           beta_tilde, 
                           alpha_prior,
                           beta_prior,
                           similarity,
                           leap_size,
                           leap) {
  
  total_trials <- dim(states)[2]
  
  n_stimulus <- dim(states)[1]
  
  current_position <- c(alpha_tilde, beta_tilde)
  
  position <- current_position
  
  momentum <- rnorm(n = length(current_position), mean = 0, sd = 1)
  
  momentum_current <- momentum
  
  momentum <- momentum - leap_size *
    gradient_inertia(states = states, 
                     alpha_tilde = alpha_tilde,
                     beta_tilde = beta_tilde,
                     alpha_prior = alpha_prior,
                     beta_prior = beta_prior,
                     similarity = similarity, 
                     total_trials = total_trials, 
                     n_stimulus = n_stimulus) / 2
  
  leap <- ceiling(runif(n = 1) * leap) - 1
  
  for (k in 1:leap) {
    
    position <- position + leap_size * momentum
    
    if (k != leap) {
      momentum <- momentum - leap_size * 
        gradient_inertia(states = states, 
                         alpha_tilde = alpha_tilde,
                         beta_tilde = beta_tilde,
                         alpha_prior = alpha_prior,
                         beta_prior = beta_prior,
                         similarity = similarity, 
                         total_trials = total_trials, 
                         n_stimulus = n_stimulus)
    }
  }
  
  momentum <- momentum - leap_size * 
    gradient_inertia(states = states, 
                     alpha_tilde = alpha_tilde,
                     beta_tilde = beta_tilde,
                     alpha_prior = alpha_prior,
                     beta_prior = beta_prior,
                     similarity = similarity, 
                     total_trials = total_trials, 
                     n_stimulus = n_stimulus) / 2
  
  momentum <- -momentum
  
  kinetic_current <- sum(momentum_current^2) / 2
  
  kinetic_proposed <- sum(momentum^2) / 2
  
  potential_current <- kinetic_current - 
    log_posterior(states = states, 
                  alpha_tilde = current_position[1],
                  beta_tilde = current_position[2],
                  alpha_prior = alpha_prior,
                  beta_prior = beta_prior,
                  similarity = similarity,
                  total_trials = total_trials,
                  n_stimulus = n_stimulus) 
  
  potential_proposed <- kinetic_proposed -
    log_posterior(states = states, 
                  alpha_tilde = position[1],
                  beta_tilde = position[2],
                  alpha_prior = alpha_prior,
                  beta_prior = beta_prior,
                  similarity = similarity,
                  total_trials = total_trials,
                  n_stimulus = n_stimulus)
  
  
  acceptance_probability <- min(1, exp(potential_current - potential_proposed))

  ifelse(test = is.finite(exp(position[1])) & is.finite(exp(position[2])),
         yes = ifelse(test = runif(n = 1) < acceptance_probability, 
                      yes = return(list(position, 1)),
                      no = return(list(current_position, 0))),
         no = return(list(current_position, 0)))
}
