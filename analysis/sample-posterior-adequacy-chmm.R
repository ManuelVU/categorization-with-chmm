# Calculate posterior adequacy by trial and participant for Bartlema's data
# using chmm model

bart <- readr::read_csv(file = "data/csv-files/bartlema-diagonal.csv")

samples <- readRDS(file = "data/posterior-samples/bartlema-diagonal-posterior-samples.rds")

source(file = "src/sampling-f/posterior-adequacy-sampler.R")

participants <- unique(bart$id)

pp_count <- 0

mode_y <- matrix(NA, nrow = length(participants), 
                 ncol = max(bart$trial))

for (p in participants){
  pp_count <- pp_count + 1
  
  y_hat_tmp <- posterior_adequacy_transfers(data = bart, 
                                            posterior = samples, 
                                            participant_id = 
                                              which(participants == p))
  
  mode_y[pp_count, ] <- colMeans(y_hat_tmp)
  
}

saveRDS(object = mode_y, 
        file = "data/posterior-samples/posterior-predictive-bartlema-diagonal/chmm-all.rds")
