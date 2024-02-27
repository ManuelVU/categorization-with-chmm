################################################################################
## This function calculates the Brier score using the mean of the posterior   ##
## predictive distribution and the observations.                              ##
################################################################################

participant_brier <- function (data,
                               posterior_mean_response,
                               participant_id,
                               trial_participant) {
  
  outcome <- data$response[data$id == participant_id]
  outcome <- outcome[1:trial_participant]
  
  prediction <- 
    posterior_mean_response[which(unique(data$id) == participant_id),
                            1:trial_participant]
  
  
  brier <- (prediction - outcome)^2
  
  return(brier)
  
}