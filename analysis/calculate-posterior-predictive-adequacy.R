# Code to plot the horizontal histograms with the posterior predictive accuracy
# of the gcm and chmm models

# Load data from diagonal condition in bartlema
bart <- readr::read_csv(file = "data/csv-files/bartlema-diagonal.csv")

# Load posterior predictive accuracy function
source(file = "src/summary-f/posterior-predictive-accuracy.R")

# Calculate posterior predictive accuracy for each participant and 4 of the 
# transfer trials
pp_acc <- posterior_pred_accuracy(
  chmm_path = "data/posterior-samples/posterior-predictive-bartlema-diagonal/chmm-transfer-", 
  gcm_path = "data/posterior-samples/posterior-predictive-bartlema-diagonal/gcm-transfer-", 
  data = bart)

# Calculate mean accuracy by participant
pp_acc$mean_chmm <- apply(X = pp_acc$chmm, MARGIN = c(1, 2), FUN = mean)

pp_acc$mean_gcm <- apply(X = pp_acc$gcm, MARGIN = c(1, 2), FUN = mean)

# Save posterior predictive accuracy of each model on a separate file

saveRDS(object = pp_acc, 
        file = "data/posterior-samples/posterior-predictive-bartlema-diagonal/accuracy.rds")
