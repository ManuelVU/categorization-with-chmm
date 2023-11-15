################################################################################
# Function that adjusts the step size in the HMC algorithm. This function is 
# used during the CHMM sampling process.
################################################################################

# This function takes the following arguments:
#   1: step_size, current step size between iterations of the HMC algorithm.
#   2: acceptance_prob, current acceptance probability calculated as the 
#      proportion of accepted samples in the last 100 samples from the HMC.
#   3: target_acceptance, target acceptance probability for samples drawn with 
#      with the HMC algorithm.

adjust_step <- function(step_size, acceptance_prob, target_acceptance){
 
  x <- c()
  new_step_size <- c()
  
  for (i in 1:length(step_size)){
    
    x[i] <- 1 + 1000 * (acceptance_prob[i] - target_acceptance) ^ 3
    
    new_step_size[i] <- ifelse(test = x[i] < 0.9, 
                               yes = step_size[i] * 0.9, 
                               no = ifelse(test = x[i] > 1.1, 
                                           yes = step_size[i] * 1.1, 
                                           no = step_size[i]))
    
  }
  
  return(new_step_size)
}
