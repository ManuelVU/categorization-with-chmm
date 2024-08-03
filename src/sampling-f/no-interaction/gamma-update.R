################################################################################
## Function that draws a single sample of the initial probability parameter   ##
## gamma which represents the probability of the initial states               ##
################################################################################

gamma_update <- function(initial_states, gamma_prior){
  
  category_a <- colSums(x = 1 - initial_states)
  category_b <- colSums(x = initial_states)
  
  alpha_posterior <- category_b + gamma_prior[1]
  beta_posterior <- category_a + gamma_prior[2]
  
  sample <- rbeta(n = length(alpha_posterior), 
                  shape1 = alpha_posterior, shape2 = beta_posterior)
  
  return(sample)
}