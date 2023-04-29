# Function for updating the logarithm of the inertia parameters using the 
# Hamiltonian Monte Carlo algorithm

hamiltonian_mc <- function(states, alpha_tilde, beta_tilde, 
                           alpha_prior, beta_prior,
                           similarity,
                           epsilon, leap, 
                           potential_current) {
  
  total_trials <- dim(states)[2]
  
  n_stimulus <- dim(states)[1]
  
  current_position <- c(alpha_tilde, beta_tilde)
  
  position <- current_position
  
  momentum <- rnorm(n = length(current_position), mean = 0, sd = 1)
  
  momentum_current <- momentum
  
  momentum <- momentum - epsilon * gradient_inertia(states = states, 
                                                    alpha_tilde = alpha_tilde,
                                                    beta_tilde = beta_tilde,
                                                    alpha_prior = alpha_prior,
                                                    beta_prior = beta_prior,
                                                    similarity = similarity, 
                                                    total_trials = total_trials, 
                                                    n_stimulus = n_stimulus) / 2
  
  leap <- ceiling(runif(n = 1) * leap) - 1
  
  for (k in 1:leap) {
    
    position <- position + epsilon * momentum
    
    if (k != leap) {
      momentum <- momentum - epsilon * 
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
  
  momentum <- momentum - epsilon * gradient_inertia(states = states, 
                                                    alpha_tilde = alpha_tilde,
                                                    beta_tilde = beta_tilde,
                                                    alpha_prior = alpha_prior,
                                                    beta_prior = beta_prior,
                                                    similarity = similarity, 
                                                    total_trials = total_trials, 
                                                    n_stimulus = n_stimulus) / 2
  
  momentum <- -momentum
  
  potential_proposed <- log_posterior(states = states, 
                                      alpha_tilde = position[1],
                                      beta_tilde = position[2],
                                      alpha_prior = alpha_prior,
                                      beta_prior = beta_prior,
                                      similarity = similarity,
                                      total_trials = total_trials,
                                      n_stimulus = n_stimulus)
  
  kinetic_proposed <- sum(momentum^2) / 2
  
  kinetic_current <- sum(momentum_current^2) / 2
  
  acceptance_probability <- exp(potential_current - potential_proposed +
                                  kinetic_current - kinetic_proposed)

  ifelse(test = runif(n = 1) < acceptance_probability, 
         yes = return(list(position, potential_proposed)),
         no = return(list(current_position, potential_current)))
}

# test

a <- readr::read_csv(file = "data/stimulus-features/lee-navarro-features.csv")
b <- distinctive_ln(stimulus_features = a)
d <- featural_distance(distinctive_features = b)
sm <- similarity_ij(decay_rate = 1, decay_function = 1, dissimilarity = d)

st <- matrix(rbinom(n = 9 * 5, size = 1, prob = 0.5),
       ncol = 5, nrow = 9)

lp <- log_posterior(alpha_tilde = 1, beta_tilde = 2,
              states = st,
              total_trials = 5, n_stimulus = 9, similarity = sm,
              alpha_prior = c(1, 1), beta_prior = c(1, 1))

hamiltonian_mc(states = st, alpha_tilde = 1, beta_tilde = 2, 
               alpha_prior = c(1, 1), beta_prior = c(1, 1),
               similarity = sm, epsilon = 0.05, leap = 20,
               potential_current = lp)
