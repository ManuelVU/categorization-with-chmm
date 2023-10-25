# This code takes the posterior samples of the CHMM before the first transfer
# phase of the diagonal design in Bartlema et al.

#### Transfer 1 ----
# Load functions into R environment
rm(list = ls())
gc()

source(file = "analysis/load-functions.R")

# Generate the data needed for the sampler using Lee and Navarro's data set
bartlema <- transform_data_chmm(
  directory_data = "data/csv-files/bartlema-diagonal-filtered.csv",
  directory_features = "data/stimulus-features/bartlema-Diagonal-features.csv")

transfer_trial <- readr::read_csv(
  file = "data/csv-files/bartlema-diagonal-filtered.csv")
transfer_trial <- unique(subset(x = transfer_trial, 
                         subset = condition_char == "transfer" & 
                           id == 4)$trial_condition)

bartlema$response <- bartlema$response[, 1:(transfer_trial[1] - 1), ]
bartlema$participant_t <- rep(x = (transfer_trial[1] - 1), 
                              times = length(bartlema$participant_t))

# Start constants used by the sampler
iterations <- 15000
burn <- 10000
acceptance_target <- 0.8
cores <- as.integer(round(x = parallel::detectCores() / 2, digits = 0))

prior_values <- list("gamma" = c(1, 1),
                     "epsilon" = c(10, 888),
                     "alpha" = c(2, 1), 
                     "beta" = c(2, 1))

initial_values <- list("gamma" = rep(x = 0.5, 
                                     times = dim(bartlema$response)[3]),
                       "epsilon" = rbeta(n = dim(bartlema$response)[3],
                                         shape1 = 10,
                                         shape2 = 888),
                       "alpha" = rgamma(n = dim(bartlema$response)[3],
                                        shape = 2, rate = 1),
                       "beta" = rgamma(n = dim(bartlema$response)[3],
                                       shape = 2, rate = 1))

step_size_starting <- rep(0.0015, dim(bartlema$response)[3])

# Start sampling using chmm_sampler
samples <- chmm_sampling(data_chmm = bartlema,
                         metric = "minkowski",
                         order_p = 1,
                         n_iterations = iterations,
                         n_burn = burn,
                         n_cores = cores,
                         parameters_initial_values = initial_values,
                         prior_parameters = prior_values,
                         start_step_size = step_size_starting, 
                         hmc_acceptance = acceptance_target)

# Save posterior samples
saveRDS(object = samples, 
        file = paste(c("data/posterior-samples/model-parameters/",
                       "bartlema-diagonal-filtered-transfer1-chmm.rds"), 
                     collapse = ""))

#### Transfer 3 ----
# Load functions into R environment
rm(list = ls())
gc()
source(file = "analysis/load-functions.R")

# Generate the data needed for the sampler using Lee and Navarro's data set
bartlema <- transform_data_chmm(
  directory_data = "data/csv-files/bartlema-diagonal-filtered.csv",
  directory_features = "data/stimulus-features/bartlema-Diagonal-features.csv")

transfer_trial <- readr::read_csv(
  file = "data/csv-files/bartlema-diagonal-filtered.csv")
transfer_trial <- unique(subset(x = transfer_trial, 
                                subset = condition_char == "transfer" & 
                                  id == 4)$trial_condition)

bartlema$response <- bartlema$response[, 1:(transfer_trial[3] - 1), ]
bartlema$participant_t <- rep(x = (transfer_trial[3] - 1), 
                              times = length(bartlema$participant_t))

# Start constants used by the sampler
iterations <- 15000
burn <- 10000
acceptance_target <- 0.8
cores <- as.integer(round(x = parallel::detectCores() / 2, digits = 0))

prior_values <- list("gamma" = c(1, 1),
                     "epsilon" = c(10, 888),
                     "alpha" = c(2, 1), 
                     "beta" = c(2, 1))

initial_values <- list("gamma" = rep(x = 0.5, 
                                     times = dim(bartlema$response)[3]),
                       "epsilon" = rbeta(n = dim(bartlema$response)[3],
                                         shape1 = 10,
                                         shape2 = 888),
                       "alpha" = rgamma(n = dim(bartlema$response)[3],
                                        shape = 2, rate = 1),
                       "beta" = rgamma(n = dim(bartlema$response)[3],
                                       shape = 2, rate = 1))

step_size_starting <- rep(0.0015, dim(bartlema$response)[3])

# Start sampling using chmm_sampler
samples <- chmm_sampling(data_chmm = bartlema,
                         metric = "minkowski",
                         order_p = 1,
                         n_iterations = iterations,
                         n_burn = burn,
                         n_cores = cores,
                         parameters_initial_values = initial_values,
                         prior_parameters = prior_values,
                         start_step_size = step_size_starting, 
                         hmc_acceptance = acceptance_target)

# Save posterior samples
saveRDS(object = samples, 
        file = paste(c("data/posterior-samples/model-parameters/",
                       "bartlema-diagonal-filtered-transfer3-chmm.rds"), 
                     collapse = ""))

#### Transfer 5 ----
# Load functions into R environment
rm(list = ls())
gc()
source(file = "analysis/load-functions.R")

# Generate the data needed for the sampler using Lee and Navarro's data set
bartlema <- transform_data_chmm(
  directory_data = "data/csv-files/bartlema-diagonal-filtered.csv",
  directory_features = "data/stimulus-features/bartlema-Diagonal-features.csv")

transfer_trial <- readr::read_csv(
  file = "data/csv-files/bartlema-diagonal-filtered.csv")
transfer_trial <- unique(subset(x = transfer_trial, 
                                subset = condition_char == "transfer" & 
                                  id == 4)$trial_condition)

bartlema$response <- bartlema$response[, 1:(transfer_trial[5] - 1), ]
bartlema$participant_t <- rep(x = (transfer_trial[5] - 1), 
                              times = length(bartlema$participant_t))

# Start constants used by the sampler
iterations <- 15000
burn <- 10000
acceptance_target <- 0.8
cores <- as.integer(round(x = parallel::detectCores() / 2, digits = 0))

prior_values <- list("gamma" = c(1, 1),
                     "epsilon" = c(10, 888),
                     "alpha" = c(2, 1), 
                     "beta" = c(2, 1))

initial_values <- list("gamma" = rep(x = 0.5, 
                                     times = dim(bartlema$response)[3]),
                       "epsilon" = rbeta(n = dim(bartlema$response)[3],
                                         shape1 = 10,
                                         shape2 = 888),
                       "alpha" = rgamma(n = dim(bartlema$response)[3],
                                        shape = 2, rate = 1),
                       "beta" = rgamma(n = dim(bartlema$response)[3],
                                       shape = 2, rate = 1))

step_size_starting <- rep(0.0015, dim(bartlema$response)[3])

# Start sampling using chmm_sampler
samples <- chmm_sampling(data_chmm = bartlema,
                         metric = "minkowski",
                         order_p = 1,
                         n_iterations = iterations,
                         n_burn = burn,
                         n_cores = cores,
                         parameters_initial_values = initial_values,
                         prior_parameters = prior_values,
                         start_step_size = step_size_starting, 
                         hmc_acceptance = acceptance_target)

# Save posterior samples
saveRDS(object = samples, 
        file = paste(c("data/posterior-samples/model-parameters/",
                       "bartlema-diagonal-filtered-transfer5-chmm.rds"), 
                     collapse = ""))

#### Transfer 7 ----
# Load functions into R environment
rm(list = ls())
gc()
source(file = "analysis/load-functions.R")

# Generate the data needed for the sampler using Lee and Navarro's data set
bartlema <- transform_data_chmm(
  directory_data = "data/csv-files/bartlema-diagonal-filtered.csv",
  directory_features = "data/stimulus-features/bartlema-Diagonal-features.csv")

transfer_trial <- readr::read_csv(
  file = "data/csv-files/bartlema-diagonal-filtered.csv")
transfer_trial <- unique(subset(x = transfer_trial, 
                                subset = condition_char == "transfer" & 
                                  id == 4)$trial_condition)

bartlema$response <- bartlema$response[, 1:(transfer_trial[7] - 1), ]
bartlema$participant_t <- rep(x = (transfer_trial[7] - 1), 
                              times = length(bartlema$participant_t))

# Start constants used by the sampler
iterations <- 15000
burn <- 10000
acceptance_target <- 0.8
cores <- as.integer(round(x = parallel::detectCores() / 2, digits = 0))

prior_values <- list("gamma" = c(1, 1),
                     "epsilon" = c(10, 888),
                     "alpha" = c(2, 1), 
                     "beta" = c(2, 1))

initial_values <- list("gamma" = rep(x = 0.5, 
                                     times = dim(bartlema$response)[3]),
                       "epsilon" = rbeta(n = dim(bartlema$response)[3],
                                         shape1 = 10,
                                         shape2 = 100),
                       "alpha" = rgamma(n = dim(bartlema$response)[3],
                                        shape = 2, rate = 1),
                       "beta" = rgamma(n = dim(bartlema$response)[3],
                                       shape = 2, rate = 1))

step_size_starting <- rep(0.0015, dim(bartlema$response)[3])

# Start sampling using chmm_sampler
samples <- chmm_sampling(data_chmm = bartlema,
                         metric = "minkowski",
                         order_p = 1,
                         n_iterations = iterations,
                         n_burn = burn,
                         n_cores = cores,
                         parameters_initial_values = initial_values,
                         prior_parameters = prior_values,
                         start_step_size = step_size_starting, 
                         hmc_acceptance = acceptance_target)

# Save posterior samples
saveRDS(object = samples, 
        file = paste(c("data/posterior-samples/model-parameters/",
                       "bartlema-diagonal-filtered-transfer7-chmm.rds"), 
                     collapse = ""))
