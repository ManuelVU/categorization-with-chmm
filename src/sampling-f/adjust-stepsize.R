# Function to adjust the step size of the HMC sampling algorithm

adjust_step <- function(step_size, acceptance_prob, target_acceptance){
 
   x <- 1 + 1000 * (acceptance_prob - target_acceptance) ^ 3
   
   print(x)
  
  return(ifelse(test = x < 0.9, 
                yes = step_size * 0.9,
                no = ifelse(test = x > 1.1, 
                            yes = step_size * 1.1, 
                            no = step_size)))
}