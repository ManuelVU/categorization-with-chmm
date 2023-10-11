# Calculate posterior adequacy by trial and participant for Bartlema's 
# Criss-Cross design using the CHMM model

#### Criss - Cross design ----
bart <- readr::read_csv(file = "data/csv-files/bartlema-crisscross-filtered.csv")

samples <- readRDS(file = paste(c("data/posterior-samples/model-parameters/",
                                  "bartlema-crisscross-filtered-chmm.rds"),
                                collapse = ""))

source(file = "src/sampling-f/posterior-adequacy-sampler.R")

participants <- unique(bart$id)

pp_count <- 0

posterior_add <- matrix(NA, 
                        nrow = length(participants_keep), 
                        ncol = max(bart$trial_condition))

for (p in participants){
  pp_count <- pp_count + 1
  
  participant_trials <- max(bart$trial_condition[bart$id == p])
  
  y_hat_tmp <- posterior_adequacy_transfers(data = bart, 
                                            posterior = samples, 
                                            participant_id = 
                                              which(participants == p))
  
  posterior_add[count, 1:participant_trials] <- 
    bart$response[bart$id == p] * colMeans(y_hat_tmp) +
    (1 - bart$response[bart$id == p]) * (1 - colMeans(y_hat_tmp))
  
}

saveRDS(object = posterior_add,
        file = paste(c("data/posterior-samples/posterior-adequacy/",
                       "bartlema-crisscross-filtered-chmm.rds"),
                     collapse = ""))
