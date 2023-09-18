# Posterior adequacy figure for CHMM approach in Lee and Navarro 2002

# Load type 4 category structure data from L&N 2002
lee_navarro <- readr::read_csv(file = "data/csv-files/lee-navarro-2002-type4.csv")

# Load posterior samples from CHMM model
samples <- readRDS(file = "data/posterior-samples/lee-navarro-2002-type4-posterior-samples-2.rds")

# Load posterior adequacy sampler function
source(file = "src/sampling-f/posterior-adequacy-sampler.R")

# Use all samples to calculate the mode response for each participant and trial
participants_keep <- unique(lee_navarro$id)

count <- 0

mode_y <- matrix(NA, nrow = length(participants_keep), 
                 ncol = max(lee_navarro$trial_condition))

for (p in participants_keep){
  count <- count + 1
  
  y_hat_tmp <- posterior_adequacy(posterior = samples, 
                       stimulus_id = lee_navarro$stimulus[lee_navarro$id == p], 
                       participant_id = which(participants_keep == p), 
                       total_trials = 
                         max(lee_navarro$trial_condition[lee_navarro$id == p]))
  
  mode_y[count, 1:max(lee_navarro$trial_condition[lee_navarro$id == p])] <- 
    round(colMeans(y_hat_tmp),0)
    
}
