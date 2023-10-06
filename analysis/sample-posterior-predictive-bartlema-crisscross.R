# load data to look for specific trial
bart <- readr::read_csv(file = "data/csv-files/bartlema-crisscross.csv")

# load posterior samples
samples <- readRDS(file = "data/posterior-samples/bartlema-crisscross-posterior-samples.rds")

# load functions
source(file = "analysis/load-functions.R")

# load similarity matrix

features <- readr::read_csv(file = "data/stimulus-features/bartlema-CrissCross-features.csv")

stimulus_distance <- minkowski_distance(stimulus_features = features, p = 1)

stimulus_similarity <- similarity_ij(decay_rate = 1, decay_function = 1, 
                                     dissimilarity = stimulus_distance)


post_pred <- posterior_predictive(iterations = 5000, posterior = samples, 
                     similarity = stimulus_similarity, 
                     trial = 32, participant_id = 1)

saveRDS(object = post_pred, 
        file = "data/posterior-samples/bartlema-crisscross-posterior-predictive-transfer-1.rds")
