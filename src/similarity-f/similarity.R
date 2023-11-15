################################################################################
# Function that calculates the similarity between stimulus I and J based on the 
# using the featural distance metric from Lee and Navarro (2002).
################################################################################

# This function takes three arguments 
#   1: decay_rate, rate of similarity decay as a function of distance. 
#   2: decay_function, functional form of the generalization function.
#   3: dissimilarity, a symmetric dissimilarity matrix.

similarity_ij <- function(decay_rate = 1, decay_function = 1, dissimilarity){
  exp(-decay_rate * (dissimilarity ^ decay_function))
} 
