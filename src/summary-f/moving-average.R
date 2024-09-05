################################################################################
## Function that computes a moving average with a fixed window size           ##
################################################################################

moving_average <- function(data, window_size){
  t <- length(data)
  ma <- c()
  for(i in 1:t){
    if(i < window_size){
      ma[i] <- mean(data[1:i], na.rm = TRUE) 
    }
    else{
      ma[i] <- mean(data[(i - window_size + 1):i], na.rm = TRUE)
    }
  }
  return(ma)
}
