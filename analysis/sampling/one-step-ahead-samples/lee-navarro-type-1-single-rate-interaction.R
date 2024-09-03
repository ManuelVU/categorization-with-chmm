################################################################################
## The code in this section obtains the posterior one-step-ahead agreement    ##
## between participant's responses and model posterior samples for a CHMM.    ##
## with a single rate and stimulus interaction for type 1 condition           ##
################################################################################

# remove data from environment including hidden objects
remove(list = ls())

# clear ram
gc()

# load functoins from the one rate interaction model
source(file = "analysis/load-functions-single-rate-interaction.R")

# load stimulus features from csv files
features <- readr::read_csv(
  file = "data/stimulus-features/lee-navarro-features.csv")

# load data from type 1 condition from csv file
type_1 <- readr::read_csv(
  file = "data/csv-files/lee-navarro-2002-type1-filtered.csv")

# load posterior samples
samples_t1 <- readRDS(
  file = paste(x = c("data/posterior-samples/model-parameters/",
                     "lee-navarro-type1-filtered-chmm-stick-weight.rds"),
               collapse = ""))

# calculate distinctiveness of stimuli in the task
stimulus_distinctive <- distinctive_ln(stimulus_features = features)

# calculate distance matrix based on stimulus distinctiveness
stimulus_distance <- featural_distance(
  distinctive_features = stimulus_distinctive) 

# calculate similarity matrix based on featural distance
stimulus_similarity <- similarity_ij(decay_rate = 1, 
                                     decay_function = 1, 
                                     dissimilarity = stimulus_distance)

# One step ahead posterior adequacy
posterior_one_step <- one_step_ahead(data = type_1,
                                     max_trials = max(type_1$trial),
                                     samples = samples_t1,
                                     n_samples = 100,
                                     similarity = stimulus_similarity)
