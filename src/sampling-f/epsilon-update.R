# Function that takes a single sample of the conditional posterior distribution
# of the error parameter in the categorization model. The error parameter
# starts with a beta prior.
epsilon_update <- function(states_all, responses_all, epsilon_prior){
  error_s0 <- (states_all == 0 & responses_all == 1) |> 
    apply(MARGIN = 3, FUN = sum, na.rm = TRUE)
  
  error_s1 <- (states_all == 1 & responses_all == 0) |> 
    apply(MARGIN = 3, FUN = sum, na.rm = TRUE)
  
  correct_s0 <- (states_all == 0 & responses_all == 0) |> 
    apply(MARGIN = 3, FUN = sum, na.rm = TRUE)
  
  correct_s1 <- (states_all == 1 & responses_all == 1) |> 
    apply(MARGIN = 3, FUN = sum, na.rm = TRUE)
  
  print(epsilon_prior[1] + error_s0[2] + error_s1[2])
  print(epsilon_prior[2] + correct_s0[2] + correct_s1[2])
  
  return(rbeta(n = length(correct_s1), 
               shape1 = epsilon_prior[1] + error_s0 + error_s1,
               shape2 = epsilon_prior[2] + correct_s0 + correct_s1))
}
