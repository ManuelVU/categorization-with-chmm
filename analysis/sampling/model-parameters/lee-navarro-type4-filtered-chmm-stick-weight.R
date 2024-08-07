# This code is for testing the functions with the two data sets

# remove variables from the environment
rm(list = ls())

# clear ram
gc()

# load functions into r environment
source(file = "analysis/load-functions-single-rate-interaction.R")

# load data from lee and navarro type four condition into chmm format
lee_navarro <- transform_data_chmm(
  directory_data = "data/csv-files/lee-navarro-2002-type4-filtered.csv",
  directory_features = "data/stimulus-features/lee-navarro-features.csv")

# set number of iterations
iterations <- 20000

# set number of burn in samples
burn <- 12000

# set acceptance rate target for HMC
acceptance_target <- 0.8

# set number of cores to use
cores <- as.integer(round(x = parallel::detectCores() / 2, digits = 0))

# prior values of hyperparameters in the model
prior_values <- list("gamma" = c(1, 1),
                     "epsilon" = c(2, 102),
                     "alpha" = c(2, 1), 
                     "kappa" = c(2, 1))

# initial values for the chains
initial_values <- list("gamma" = rep(x = 0.5, 
                                     times = dim(lee_navarro$response)[3]),
                       "epsilon" = rbeta(n = dim(lee_navarro$response)[3],
                                         shape1 = 10,
                                         shape2 = 888),
                       "alpha" = rep(x = 1, 
                                     times = dim(lee_navarro$response)[3]),
                       "kappa" = rep(x = 1, 
                                    times = dim(lee_navarro$response)[3]))

# set initial step size for HMC algorithm
step_size_starting <- rep(0.003, dim(lee_navarro$response)[3])

# sampling using chmm_sampler
samples <- chmm_sampling(data_chmm = lee_navarro,
                         n_iterations = iterations,
                         n_burn = burn,
                         n_cores = cores,
                         parameters_initial_values = initial_values,
                         prior_parameters = prior_values,
                         start_step_size = step_size_starting, 
                         hmc_acceptance = acceptance_target)

# save posterior samples
saveRDS(object = samples,
        file = paste(c("data/posterior-samples/model-parameters/",
                       "lee-navarro-type4-filtered-chmm-stick-weight.rds"), 
                     collapse = ""))
