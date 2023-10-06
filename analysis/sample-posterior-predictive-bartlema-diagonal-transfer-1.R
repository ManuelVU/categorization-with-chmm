# load data to look for specific trial
bart <- readr::read_csv(file = "data/csv-files/bartlema-diagonal.csv")

transfer_trial <- unique(bart$trial_condition[which(bart$condition_char == "transfer")])

# load posterior samples
samples <- readRDS(file = "data/posterior-samples/samples-transfer/bartlema-diagonal-samples-transfer-1.rds")

# load functions
source(file = "analysis/load-functions.R")

# load similarity matrix

features <- readr::read_csv(file = "data/stimulus-features/bartlema-Diagonal-features.csv")

stimulus_distance <- minkowski_distance(stimulus_features = features, p = 1)

stimulus_similarity <- similarity_ij(decay_rate = 1, decay_function = 1, 
                                     dissimilarity = stimulus_distance)

#### Transfer 1 ----

post_pred <- posterior_predictive(iterations = 5000, posterior = samples, 
                                  similarity = stimulus_similarity, 
                                  trial = (transfer_trial[1] - 1), 
                                  participant_id = 27)

saveRDS(object = post_pred, 
        file = "data/posterior-samples/posterior-predictive-bartlema-diagonal/chmm-transfer-1.rds")




