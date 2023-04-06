# Function that takes a distinctive similarity array and returns a dissimilarity 
# measure for each stimulus in the array. Dissimilarity measure is based on 
# Lee and Navarro 2002 equation 12
featural_distance <- function(saliency, distinctive_features){
  
  stopifnot(exprs = length(saliency) == dim(distinctive_features$in_i_not_in_j)[2])
  
  n_stimulus <- dim(distinctive_features$in_i_not_in_j)[3]
  
  ftd_ij <- 
    t(apply(X = distinctive_features$in_i_not_in_j, MARGIN = 3, 
                    saliency, FUN = "%*%")) + 
    t(apply(X = distinctive_features$not_in_i_in_j, MARGIN = 3, 
                                                    saliency, FUN = "%*%"))
  
  return(ftd_ij)
}
