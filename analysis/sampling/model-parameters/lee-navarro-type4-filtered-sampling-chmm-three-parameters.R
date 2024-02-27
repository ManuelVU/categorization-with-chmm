# Load functions into R environment
source(file = "analysis/load-functions-three-parameters.R")

# load data in chmm format
lee_navarro <- transform_data_chmm(
  directory_data = "data/csv-files/lee-navarro-2002-type4-filtered.csv",
  directory_features = "data/stimulus-features/lee-navarro-features.csv")

# set number of iterations for the sampler
iterations <- 25000

# set number of burn in samples 
# (this will be used as an adaptation period by the HMC algorithm)
burn <- 15000

# set target acceptance ratio for HMC algorithm
acceptance_target <- 0.7

# set number of cores used for parallel computing
cores <- as.integer(round(x = parallel::detectCores() / 2, digits = 0))

# set values of the prior distributions for the sampler, distributions used 
# are explained in the paper
prior_values <- list("gamma" = c(1, 1),
                     "epsilon" = c(2, 102),
                     "alpha" = c(2, 1), 
                     "beta" = c(2, 1),
                     "kappa" = c(2, 1))

# set initial values for the chains, the initial values for the hidden states 
# are started by the sampler itself
initial_values <- list("gamma" = rep(x = 0.5, 
                                     times = dim(lee_navarro$response)[3]),
                       "epsilon" = rbeta(n = dim(lee_navarro$response)[3],
                                         shape1 = 2,
                                         shape2 = 102),
                       "alpha" = rep(x = 5, 
                                     times = dim(lee_navarro$response)[3]),
                       "beta" = rep(x = 5, 
                                    times = dim(lee_navarro$response)[3]),
                       "kappa" = rep(x = 1, 
                                     times = dim(lee_navarro$response)[3]))

# set step size for the adaptation method of the HMC algorithm
step_size_starting <- rep(0.1, dim(lee_navarro$response)[3])

# Start sampling using chmm_sampler
samples <- chmm_sampling(data_chmm = lee_navarro,
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
                       "lee-navarro-type4-filtered-chmm-three-parameters.rds"), 
                     collapse = ""))
