# Function that draws a single sample of the initial probability parameter 
# gamma, the argument gamma_prior has to be a two dimensional vector with 
# (alpha, beta).

gamma_update <- function(initial_states, gamma_prior){
  
  category_a <- length(initial_states == 0)
  category_b <- length(initial_states == 1)
  
  alpha_posterior <- category_b + gamma_prior[1]
  beta_posterior <- category_a + gamma_prior[2]
  
  sample <- rbeta(n = 1, shape1 = alpha_posterior, shape2 = beta_posterior)
  
  return(sample)
}