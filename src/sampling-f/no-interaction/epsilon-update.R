################################################################################
## Function that takes a single sample of the conditional posterior           ##
## distribution of the trembling hand parameter in the CHMM model.            ##
################################################################################

epsilon_update <- function(states_all, responses_all, epsilon_prior){
  
  error_s0 <- (states_all == 0 & responses_all == 1) |> 
    apply(MARGIN = 3, FUN = sum, na.rm = TRUE)
  
  error_s1 <- (states_all == 1 & responses_all == 0) |> 
    apply(MARGIN = 3, FUN = sum, na.rm = TRUE)
  
  correct_s0 <- (states_all == 0 & responses_all == 0) |> 
    apply(MARGIN = 3, FUN = sum, na.rm = TRUE)
  
  correct_s1 <- (states_all == 1 & responses_all == 1) |> 
    apply(MARGIN = 3, FUN = sum, na.rm = TRUE)
  
  posterior_alpha <- error_s0 + error_s1
  
  posterior_beta <- correct_s0 + correct_s1
  
  sample <- rbeta(n = length(correct_s1), 
                  shape1 = epsilon_prior[1] + posterior_alpha,
                  shape2 = epsilon_prior[2] + posterior_beta)
  
  return(sample)
}
