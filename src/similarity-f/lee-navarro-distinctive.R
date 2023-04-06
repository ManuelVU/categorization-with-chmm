# Function takes a matrix of stimulus features and returns two multidimensional
# arrays named for features in stimulus i not shared with stimulus j 
# (in_i_not_in_j) and features that are not in i but are in j (not_in_i_in_j)
# distinctive features between all of them.

# Formally, the function returns a matrix with f_ik (1-f_jk) and (1-f_ik) f_jk
# with dimensions organised in multiple matrix that contain all other stimulus
# being compared in rows and features in columns
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

