# This code is for testing the functions with the two data sets

# Load functions into R environment
source(file = "analysis/load-functions.R")

# Generate the data needed for the sampler using Lee and Navarro's data set
lee_navarro <- transform_data_chmm(
  directory_data = "data/csv-files/lee-navarro-2002-type4.csv",
  directory_features = "data/stimulus-features/lee-navarro-features.csv")

# Start constants used by the sampler
iterations <- 5000
burn <- 2500
acceptance_target <- 0.4
cores <- as.integer(round(x = parallel::detectCores() / 2, digits = 0))

prior_values <- list("gamma" = c(1, 1),
                     "epsilon" = c(10, 100),
                     "alpha" = c(2, 1), 
                     "beta" = c(2, 1))

initial_values <- list("gamma" = 0.5,
                       "epsilon" = rbeta(n = dim(lee_navarro$response)[3],
                                         shape1 = 10,
                                         shape2 = 100),
                       "alpha" = rgamma(n = dim(lee_navarro$response)[3],
                                        shape = 2, rate = 1),
                       "beta" = rgamma(n = dim(lee_navarro$response)[3],
                                       shape = 2, rate = 1))

step_size_starting <- rep(0.0015, dim(lee_navarro$response)[3])

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
        file = "analysis/posterior-samples/lee-navarro-2002-type4-posterior-samples.rds")
