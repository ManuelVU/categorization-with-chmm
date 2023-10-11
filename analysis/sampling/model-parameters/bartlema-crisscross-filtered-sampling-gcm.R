# GCM model applied to the crisscross structure data in Bartlema et al

library(R2jags)

source(file = "src/sampling-f/transform-data-gcm.R")

jags_data <- transform_gcm(
  data_path = "data/csv-files/bartlema-crisscross-filtered.csv",
  features_path = "data/stimulus-features/bartlema-CrissCross-features.csv")

gcm_model <- "
data{
  for (j in 1:n_stimulus) {
    for (k in 1:n_stimulus) {
      for (z in 1:n_dimentions) {
        d[j, k, z] = abs(features[j, z] - features[k, z])
      }
    }
  }
}
model{
  for (i in 1:n_participants) {
    # Prior Distributions
    omega[i, 1:n_dimentions] ~ ddirch(rep(1, n_dimentions))
    beta[i] ~ dunif(0, 1)
    lambda[i] ~ dgamma(2, 1)
    
    for (j in 1:n_stimulus) {
      for (k in 1:n_stimulus) {
        s[i, j, k] = exp(-lambda[i] * inprod(omega[i, ],d[j, k, ]))
      }
      
      response_a[i, j] = inprod(s[i, j, ], belong_a)
      response_b[i, j] = inprod(s[i, j, ], belong_b)
      
      theta[i, j] = beta[i] * response_a[i, j] / 
                   (beta[i] * response_a[i, j] + 
                   (1 - beta[i]) * response_b[i, j])
        
      y[i, j] ~ dbin(theta[i, j], n_trials[j])
    }
  }
}
"
gcm_parameters <- c("omega", "beta", "lambda", "theta")

gcm_samples <- jags(data = jags_data, parameters.to.save = gcm_parameters,
                    model.file = textConnection(gcm_model), n.chains = 5, 
                    n.iter = 20000, n.burnin = 19000, n.thin = 1)

saveRDS(object = gcm_samples, 
        file = paste(c("data/posterior-samples/model-parameters/",
                       "bartlema-crisscross-filtered-gcm.rds"),
                     collapse = ""))
