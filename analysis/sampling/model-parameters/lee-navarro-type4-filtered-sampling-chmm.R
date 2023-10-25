# This code is for testing the functions with the two data sets

# Load functions into R environment
source(file = "analysis/load-functions.R")

# Generate the data needed for the sampler using Lee and Navarro's data set
lee_navarro <- transform_data_chmm(
  directory_data = "data/csv-files/lee-navarro-2002-type4-filtered.csv",
  directory_features = "data/stimulus-features/lee-navarro-features.csv")

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
                                     times = dim(lee_navarro$response)[3]),
                       "epsilon" = rbeta(n = dim(lee_navarro$response)[3],
                                         shape1 = 10,
                                         shape2 = 888),
                       "alpha" = rep(x = 5, 
                                     times = dim(lee_navarro$response)[3]),
                       "beta" = rep(x = 5, 
                                    times = dim(lee_navarro$response)[3]))

step_size_starting <- rep(0.003, dim(lee_navarro$response)[3])

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
                       "lee-navarro-type4-filtered-chmm.rds"), 
                     collapse = ""))
