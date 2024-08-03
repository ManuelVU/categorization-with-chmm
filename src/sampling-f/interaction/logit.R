################################################################################
# Logit function
# Function returns a single value with the logit of x 
################################################################################

# This function takes a single argument
# 1: x, scalar value.

logit <- function(x){
  
  max(0.00001, min(0.99999, 1 / (1 + exp(- x))))
  
} 
