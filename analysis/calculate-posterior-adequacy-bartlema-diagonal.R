# Code to calculate the posterior predictive adequacy 
# of the gcm and chmm models

# Load data from diagonal condition in bartlema
bart <- readr::read_csv(file = "data/csv-files/bartlema-diagonal.csv")

# Load posterior predictive accuracy function
source(file = "src/summary-f/mean-posterior-adequacy.R")

# Calculate posterior predictive accuracy for each participant and 4 of the 
# transfer trials
pp_add <- posterior_adequacy(
  chmm_path = "data/posterior-samples/posterior-predictive-bartlema-diagonal/chmm-all.rds", 
  gcm_path = "data/posterior-samples/posterior-predictive-bartlema-diagonal/gcm-all.rds", 
  data = bart)

# Calculate mean accuracy by participant
pp_add$chmm_mean_trial <- apply(X = pp_add$chmm, MARGIN = c(1, 3), 
                                FUN = mean, na.rm = TRUE)
pp_add$chmm_mean_trial_stimulus <- apply(X = pp_add$chmm_mean_trial, MARGIN = 2, 
                                         FUN = mean, na.rm = TRUE)

pp_add$gcm_mean_trial <- apply(X = pp_add$gcm, MARGIN = c(1, 3), 
                               FUN = mean, na.rm = TRUE)
pp_add$gcm_mean_trial_stimulus <- apply(X = pp_add$gcm_mean_trial, MARGIN = 2, 
                                         FUN = mean, na.rm = TRUE)

# Save posterior predictive accuracy of each model on a separate file

saveRDS(object = pp_add, 
        file = "data/posterior-samples/posterior-predictive-bartlema-diagonal/adequacy.rds")
