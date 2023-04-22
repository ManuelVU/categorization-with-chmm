# function that computes the logit of x

logit <- function(x){
  1 / (1 + exp(- x ))
} 
