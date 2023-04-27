# Function that draws a single sample of the initial probability parameter 
# gamma

gamma_update <- function(initial_states, alpha_prior, beta_prior){
  
  category_a <- length(initial_states == 0)
  category_b <- length(initial_states == 1)
  
  alpha_posterior <- category_b + alpha_prior
  beta_posterior <- category_a + beta_prior
  
  sample <- rbeta(n = 1, shape1 = alpha_posterior, beta_posterior)
  
  return(sample)
}