# This code takes the posterior samples of the CHMM before the first transfer
# phase of the diagonal design in Bartlema et al.

#### Transfer 1 ----
# Load functions into R environment
source(file = "analysis/load-functions.R")

# Generate the data needed for the sampler using Lee and Navarro's data set
bartlema <- transform_data_chmm(
  directory_data = "data/csv-files/bartlema-diagonal.csv",
  directory_features = "data/stimulus-features/bartlema-Diagonal-features.csv")

transfer_trial <- readr::read_csv(file = "data/csv-files/bartlema-diagonal.csv")
transfer_trial <- unique(transfer_trial$trial_condition[which(transfer_trial$condition_char == "transfer")])

bartlema$response <- bartlema$response[, 1:(transfer_trial[1] - 1), ]
bartlema$participant_t <- rep(x = (transfer_trial[1] - 1), 
                              times = length(bartlema$participant_t))

# Start constants used by the sampler
iterations <- 10000
burn <- 5000
acceptance_target <- 0.8
cores <- as.integer(round(x = parallel::detectCores() / 2, digits = 0))

prior_values <- list("gamma" = c(1, 1),
                     "epsilon" = c(10, 100),
                     "alpha" = c(2, 1), 
                     "beta" = c(2, 1))

initial_values <- list("gamma" = 0.5,
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
        file = "data/posterior-samples/samples-transfer/bartlema-diagonal-samples-transfer-1.rds")

#### Transfer 3 ----
# Load functions into R environment
source(file = "analysis/load-functions.R")

# Generate the data needed for the sampler using Lee and Navarro's data set
bartlema <- transform_data_chmm(
  directory_data = "data/csv-files/bartlema-diagonal.csv",
  directory_features = "data/stimulus-features/bartlema-Diagonal-features.csv")

transfer_trial <- readr::read_csv(file = "data/csv-files/bartlema-diagonal.csv")
transfer_trial <- unique(transfer_trial$trial_condition[which(transfer_trial$condition_char == "transfer")])

bartlema$response <- bartlema$response[, 1:(transfer_trial[3] - 1), ]
bartlema$participant_t <- rep(x = (transfer_trial[3] - 1), 
                              times = length(bartlema$participant_t))

# Start constants used by the sampler
iterations <- 10000
burn <- 5000
acceptance_target <- 0.8
cores <- as.integer(round(x = parallel::detectCores() / 2, digits = 0))

prior_values <- list("gamma" = c(1, 1),
                     "epsilon" = c(10, 100),
                     "alpha" = c(2, 1), 
                     "beta" = c(2, 1))

initial_values <- list("gamma" = 0.5,
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
        file = "data/posterior-samples/samples-transfer/bartlema-diagonal-samples-transfer-3.rds")

#### Transfer 5 ----
# Load functions into R environment
source(file = "analysis/load-functions.R")

# Generate the data needed for the sampler using Lee and Navarro's data set
bartlema <- transform_data_chmm(
  directory_data = "data/csv-files/bartlema-diagonal.csv",
  directory_features = "data/stimulus-features/bartlema-Diagonal-features.csv")

transfer_trial <- readr::read_csv(file = "data/csv-files/bartlema-diagonal.csv")
transfer_trial <- unique(transfer_trial$trial_condition[which(transfer_trial$condition_char == "transfer")])

bartlema$response <- bartlema$response[, 1:(transfer_trial[5] - 1), ]
bartlema$participant_t <- rep(x = (transfer_trial[5] - 1), 
                              times = length(bartlema$participant_t))

# Start constants used by the sampler
iterations <- 10000
burn <- 5000
acceptance_target <- 0.8
cores <- as.integer(round(x = parallel::detectCores() / 2, digits = 0))

prior_values <- list("gamma" = c(1, 1),
                     "epsilon" = c(10, 100),
                     "alpha" = c(2, 1), 
                     "beta" = c(2, 1))

initial_values <- list("gamma" = 0.5,
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
        file = "data/posterior-samples/samples-transfer/bartlema-diagonal-samples-transfer-5.rds")

#### Transfer 7 ----
# Load functions into R environment
source(file = "analysis/load-functions.R")

# Generate the data needed for the sampler using Lee and Navarro's data set
bartlema <- transform_data_chmm(
  directory_data = "data/csv-files/bartlema-diagonal.csv",
  directory_features = "data/stimulus-features/bartlema-Diagonal-features.csv")

transfer_trial <- readr::read_csv(file = "data/csv-files/bartlema-diagonal.csv")
transfer_trial <- unique(transfer_trial$trial_condition[which(transfer_trial$condition_char == "transfer")])

bartlema$response <- bartlema$response[, 1:(transfer_trial[7] - 1), ]
bartlema$participant_t <- rep(x = (transfer_trial[7] - 1), 
                              times = length(bartlema$participant_t))

# Start constants used by the sampler
iterations <- 10000
burn <- 5000
acceptance_target <- 0.8
cores <- as.integer(round(x = parallel::detectCores() / 2, digits = 0))

prior_values <- list("gamma" = c(1, 1),
                     "epsilon" = c(10, 100),
                     "alpha" = c(2, 1), 
                     "beta" = c(2, 1))

initial_values <- list("gamma" = 0.5,
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
        file = "data/posterior-samples/samples-transfer/bartlema-diagonal-samples-transfer-7.rds")
