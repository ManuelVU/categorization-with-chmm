################################################################################
## Code to calculate the brier score by participant using the mean of the     ##
## posterior predictive distribution                                          ##
################################################################################

# clean environment including hidden objects
rm(list = ls(all.names = TRUE))

# free RAM usage
gc()

# load experimental data
lee <- readr::read_csv(
  file = "data/csv-files/lee-navarro-2002-type4-filtered.csv")

# load mean of posterior predictive distribution
posterior_mean <- readRDS(
  file = paste(c("data/posterior-samples/posterior-adequacy/",
                 "mean-lee-navarro-type4-filtered-chmm.rds"),
               collapse = ""))

# load brier scoring function
source(file = "src/summary-f/participant-brier.R")

# select participants to keep
participants_keep <- unique(lee$id)

# start counter for rows
count <- 0

# generate object to save participants brier scores
brier <- matrix(NA, nrow = length(participants_keep), 
                ncol = max(lee$trial_condition))

# start for loop to calculate brier score fo each participant
for (p in participants_keep){
  count <- count + 1
  
  participant_trials <- max(lee$trial_condition[lee$id == p])
  
  brier_tmp <- participant_brier(data = lee, 
                                 posterior_mean_response = posterior_mean,
                                 participant_id = p,
                                 trial_participant = participant_trials)

  
  brier[count, 1:participant_trials] <- as.vector(brier_tmp)
  
}
