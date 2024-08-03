# Posterior adequacy figure for CHMM approach in Lee and Navarro 2002

rm(list = ls())

gc()

# Load type 4 category structure data from L&N 2002
lee_navarro <- readr::read_csv(
  file = "data/csv-files/lee-navarro-2002-type4-filtered.csv")

# Load posterior samples from CHMM model
samples <- readRDS(
  file = paste(c("data/posterior-samples/model-parameters/",
                 "lee-navarro-type4-filtered-chmm.rds"), 
               collapse = ""))

# Load posterior adequacy sampler function
source(file = "src/sampling-f/interaction/posterior-adequacy-sampler.R")

# Use all samples to calculate the mode response for each participant and trial
participants_keep <- unique(lee_navarro$id)

count <- 0

posterior_add <- matrix(NA, nrow = length(participants_keep), 
                 ncol = max(lee_navarro$trial_condition))

for (p in participants_keep){
  count <- count + 1
  
  participant_trials <- max(lee_navarro$trial_condition[lee_navarro$id == p])
  
  y_hat_tmp <- posterior_adequacy(posterior = samples, 
                       stimulus_id = lee_navarro$stimulus[lee_navarro$id == p], 
                       participant_id = which(participants_keep == p), 
                       total_trials = participant_trials)
  
  posterior_add[count, 1:participant_trials] <- 
    lee_navarro$response[lee_navarro$id == p] * colMeans(y_hat_tmp) +
    (1 - lee_navarro$response[lee_navarro$id == p]) * (1 - colMeans(y_hat_tmp))
    
}

saveRDS(object = posterior_add,
        file = paste(c("data/posterior-samples/posterior-adequacy/",
                       "lee-navarro-type4-filtered-chmm.rds"), 
                     collapse = ""))
