# Function takes the path to a data file and returns a list with three elements, 
# the responses as a 3 dimensional array (stimulus, trial, participant) the 
# number of trials per participant in the study and the stimulus .

transform_data_chmm <- function(directory_data, directory_features) {
  
  a <- readr::read_csv(file = directory_data)
  
  f <- readr::read_csv(file = directory_features)

  participant_id <- unique(a$id)

  stimulus_id <- unique(a$stimulus)

  response_output <- array(data = NA,
                           dim = c(length(stimulus_id),
                                   max(a$trial_condition),
                                   length(participant_id)))
  
  trial_participant <- c()
  
  count <- 0

  for (pp in participant_id) {

    b <- subset(a, subset = id == pp)

    if (sum(is.na(b$response_char)) < 1) {

      count <- count + 1

      trial_participant <- append(x = trial_participant,
                                  values = max(b$trial_condition))

      for (tt in 1:trial_participant[count]) {

        response_output[b$stimulus[tt], tt, count] <- b$response[tt]

      }
    }
  }
  
  output <- list("response" = response_output[,,1:count],
                 "participant_t" = trial_participant,
                 "stimulus_features" = f)
  
  return(output)
}
