################################################################################
# Function takes a matrix of stimulus features and returns two multidimensional 
# arrays named after features in stimulus I not shared with stimulus J 
# (in_i_not_in_j) and features that are not in I but are in J (not_in_i_in_j)
################################################################################

#---
# Formally, the function returns a matrix with f_ik (1-f_jk) and (1-f_ik) f_jk
# where k represents the feature. These are returned as two independent lists 
# containing multiple matrices.
#---

# The function takes a single argument
#   1: stimulus_features, which is a stimulus-features matrix with columns
#      | stimulus_id | stimulus_name | feature_1 | ... | feature_n |

distinctive_ln <- function(stimulus_features){
  
  n_stimulus <- nrow(stimulus_features)
  n_features <- ncol(stimulus_features) - 2
  
  output <- list("in_i_not_in_j" = array(data = NA, 
                                         dim = c(n_stimulus,
                                                 n_features, 
                                                 n_stimulus)),
                 "not_in_i_in_j" = array(data = NA, 
                                         dim = c(n_stimulus,
                                                 n_features, 
                                                 n_stimulus)))
  for(i in 1:n_stimulus){
    
    st_features <- unlist(stimulus_features[i, (3:ncol(stimulus_features))]) |> 
      as.vector() |> 
      rep(times = n_stimulus) |> 
      matrix(nrow = n_stimulus, byrow = TRUE)
    
    st_index_comp <- stimulus_features[, 1]
    
    st_features_comp <- stimulus_features[, (3:ncol(stimulus_features))] |> 
      as.matrix()
    
    output$in_i_not_in_j[,,i] <- st_features * (1 - st_features_comp)
    
    output$not_in_i_in_j[,,i] <- (1 - st_features) * st_features_comp
    
  }
  return(output)
}
