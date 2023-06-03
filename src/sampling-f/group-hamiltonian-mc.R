# Group level Hamiltonian Monte Carlo

hamiltonian_mc_group <- function(states, total_trials,
                                 alpha_tilde, beta_tilde,
                                 alpha_prior, beta_prior,
                                 similarity,
                                 leap_size, leap){
  n_stimulus <- dim(states)[1]
  
  current_position <- c(alpha_tilde, beta_tilde)
  
  position <- current_position
  
  momentum <- rnorm(n = length(current_position), mean = 0, sd = 1)
  
  momentum_current <- momentum
  
  momentum <- momentum - leap_size * 
    gradient_inertia_group(states = states, 
                           total_trials = total_trials, 
                           alpha_tilde = alpha_tilde,
                           beta_tilde = beta_tilde,
                           alpha_prior = alpha_prior,
                           beta_prior = beta_prior,
                           similarity = similarity, 
                           n_stimulus = n_stimulus) / 2
  
  leap <- ceiling(runif(n = 1) * leap) - 1
  
  for (k in 1:leap) {
    
    position <- position + leap_size * momentum
    
    if (k != leap) {
      momentum <- momentum - leap_size * 
        gradient_inertia_group(states = states, 
                               total_trials = total_trials, 
                               alpha_tilde = alpha_tilde,
                               beta_tilde = beta_tilde,
                               alpha_prior = alpha_prior,
                               beta_prior = beta_prior,
                               similarity = similarity, 
                               n_stimulus = n_stimulus)
    }
  }
  
  momentum <- momentum - leap_size * 
    gradient_inertia_group(states = states, 
                           total_trials = total_trials, 
                           alpha_tilde = alpha_tilde,
                           beta_tilde = beta_tilde,
                           alpha_prior = alpha_prior,
                           beta_prior = beta_prior,
                           similarity = similarity, 
                           n_stimulus = n_stimulus) / 2
  
  momentum <- -momentum
  
  potential_current <- log_posterior_group(states = states, 
                                     total_trials = total_trials,
                                     alpha_tilde = current_position[1],
                                     beta_tilde = current_position[2],
                                     alpha_prior = alpha_prior,
                                     beta_prior = beta_prior,
                                     similarity = similarity,
                                     n_stimulus = n_stimulus)
  
  potential_proposed <- log_posterior_group(states = states, 
                                      total_trials = total_trials,
                                      alpha_tilde = position[1],
                                      beta_tilde = position[2],
                                      alpha_prior = alpha_prior,
                                      beta_prior = beta_prior,
                                      similarity = similarity,
                                      n_stimulus = n_stimulus)
  
  kinetic_proposed <- sum(momentum^2) / 2
  
  kinetic_current <- sum(momentum_current^2) / 2
  
  log_acceptance_probability <- potential_current - potential_proposed + 
                            kinetic_current - kinetic_proposed
  
  ifelse(test = is.finite(exp(position[1])) & is.finite(exp(position[2])),
         yes = ifelse(test = log(runif(n = 1)) < log_acceptance_probability, 
                      yes = return(list(position, 1)),
                      no = return(list(current_position, 0))),
         no = return(list(current_position, 0)))
  
}

a <- readr::read_csv(file = "data/stimulus-features/lee-navarro-features.csv")
b <- distinctive_ln(stimulus_features = a)
d <- featural_distance(distinctive_features = b)
sm <- similarity_ij(decay_rate = 1, decay_function = 1, dissimilarity = d)
# 
st <- array(rbinom(n = 9 * 5 * 10, size = 1, prob = 0.5),
       dim = c(9,5,10))
# 
# lp <- log_posterior(alpha_tilde = 1, beta_tilde = 2,
#               states = st,
#               total_trials = 5, n_stimulus = 9, similarity = sm,
#               alpha_prior = c(1, 1), beta_prior = c(1, 1))
# 
hamiltonian_mc_group(states = st, total_trials = rep(5,10), 
                     alpha_tilde = 1, beta_tilde = 2,
               alpha_prior = c(2, 1), beta_prior = c(2, 1),
               similarity = sm, leap_size = 0.3, leap = 30)

