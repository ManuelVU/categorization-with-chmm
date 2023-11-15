################################################################################
# Function that returns a single sample of the posterior distribution of the 
# initial state probability parameter.
# The function returns a single sample of the conditional posterior distribution 
# of the initial probability parameter for each participant as a p-dimensional 
# vector
################################################################################

# The function takes two arguments
#   1: initial_states, matrix of initial states for all stimulus in rows and 
#      participants in columns.
#   2: gamma_prior, two dimensional vector with the parameters of the prior 
#      distribution of the initial state probability parameter

gamma_update <- function(initial_states, gamma_prior){
  
  category_a <- colSums(x = 1 - initial_states)
  category_b <- colSums(x = initial_states)
  
  alpha_posterior <- category_b + gamma_prior[1]
  beta_posterior <- category_a + gamma_prior[2]
  
  sample <- rbeta(n = length(alpha_posterior), 
                  shape1 = alpha_posterior, shape2 = beta_posterior)
  
  return(sample)
}