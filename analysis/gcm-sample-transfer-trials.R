# GCM model applied to four different test phases of Bartlema et al.

library(R2jags)

source(file = "src/sampling-f/transform-data-gcm.R")

#### Transfer 1 ----
jags_data <- transform_gcm(data_path = "data/csv-files/bartlema-diagonal.csv", 
                           features_path = "data/stimulus-features/bartlema-Diagonal-features.csv",
                           transfer_id = 1)

jags_data$n_training <- jags_data$n_stimulus / 2

jags_data$training <- which(jags_data$belong_a == 1 | jags_data$belong_b == 1)

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
      
      theta[i, j] = beta[i] * response_b[i, j] / 
        ((1 - beta[i]) * response_a[i, j] + beta[i] * response_b[i, j])
    }
    for (t in 1:n_training) {
      y[i, t] ~ dbin(theta[i, training[t]], n_trials[training[t]])
    }
  }
}
"

gcm_parameters <- c("omega", "beta", "lambda", "theta")

gcm_samples <- jags(data = jags_data, parameters.to.save = gcm_parameters,
                    model.file = textConnection(gcm_model), n.chains = 5, 
                    n.iter = 10000, n.burnin = 9000, n.thin = 1)

theta <- gcm_samples$BUGSoutput$mean$theta

saveRDS(object = theta, 
        file = "data/posterior-samples/posterior-predictive-bartlema-diagonal/gcm-transfer-1.rds")

#### Transfer 3 ----

library(R2jags)

source(file = "src/sampling-f/transform-data-gcm.R")

jags_data <- transform_gcm(data_path = "data/csv-files/bartlema-diagonal.csv", 
                           features_path = "data/stimulus-features/bartlema-Diagonal-features.csv",
                           transfer_id = 3)

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
      
      theta[i, j] = beta[i] * response_b[i, j] / 
        ((1 - beta[i]) * response_a[i, j] + beta[i] * response_b[i, j])
        
      y[i, j] ~ dbin(theta[i, j], n_trials[j])
    }
  }
}
"
gcm_parameters <- c("omega", "beta", "lambda", "theta")

gcm_samples <- jags(data = jags_data, parameters.to.save = gcm_parameters,
                    model.file = textConnection(gcm_model), n.chains = 5, 
                    n.iter = 10000, n.burnin = 9000, n.thin = 1)

theta <- gcm_samples$BUGSoutput$mean$theta

saveRDS(object = theta, 
        file = "data/posterior-samples/posterior-predictive-bartlema-diagonal/gcm-transfer-3.rds")

# Transfer 5 ----

library(R2jags)

source(file = "src/sampling-f/transform-data-gcm.R")

jags_data <- transform_gcm(data_path = "data/csv-files/bartlema-diagonal.csv", 
                           features_path = "data/stimulus-features/bartlema-Diagonal-features.csv",
                           transfer_id = 5)

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
      
      theta[i, j] = beta[i] * response_b[i, j] / 
        ((1 - beta[i]) * response_a[i, j] + beta[i] * response_b[i, j])
        
      y[i, j] ~ dbin(theta[i, j], n_trials[j])
    }
  }
}
"
gcm_parameters <- c("omega", "beta", "lambda", "theta")

gcm_samples <- jags(data = jags_data, parameters.to.save = gcm_parameters,
                    model.file = textConnection(gcm_model), n.chains = 5, 
                    n.iter = 10000, n.burnin = 9000, n.thin = 1)

theta <- gcm_samples$BUGSoutput$mean$theta

saveRDS(object = theta, 
        file = "data/posterior-samples/posterior-predictive-bartlema-diagonal/gcm-transfer-5.rds")

# Transfer 7 ----

library(R2jags)

source(file = "src/sampling-f/transform-data-gcm.R")

jags_data <- transform_gcm(data_path = "data/csv-files/bartlema-diagonal.csv", 
                           features_path = "data/stimulus-features/bartlema-Diagonal-features.csv",
                           transfer_id = 7)

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
      
      theta[i, j] = beta[i] * response_b[i, j] / 
        ((1 - beta[i]) * response_a[i, j] + beta[i] * response_b[i, j])
        
      y[i, j] ~ dbin(theta[i, j], n_trials[j])
    }
  }
}
"
gcm_parameters <- c("omega", "beta", "lambda", "theta")

gcm_samples <- jags(data = jags_data, parameters.to.save = gcm_parameters,
                    model.file = textConnection(gcm_model), n.chains = 5, 
                    n.iter = 10000, n.burnin = 9000, n.thin = 1)

theta <- gcm_samples$BUGSoutput$mean$theta

saveRDS(object = theta, 
        file = "data/posterior-samples/posterior-predictive-bartlema-diagonal/gcm-transfer-7.rds")


#### All trials ----
library(R2jags)

source(file = "src/sampling-f/transform-data-gcm.R")

jags_data <- transform_gcm(data_path = "data/csv-files/bartlema-diagonal.csv", 
                           features_path = "data/stimulus-features/bartlema-Diagonal-features.csv")

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
      
      theta[i, j] = beta[i] * response_b[i, j] / 
        ((1 - beta[i]) * response_a[i, j] + beta[i] * response_b[i, j])
        
      y[i, j] ~ dbin(theta[i, j], n_trials[j])
    }
  }
}
"
gcm_parameters <- c("omega", "beta", "lambda", "theta")

gcm_samples <- jags(data = jags_data, parameters.to.save = gcm_parameters,
                    model.file = textConnection(gcm_model), n.chains = 5, 
                    n.iter = 10000, n.burnin = 9000, n.thin = 1)

theta <- gcm_samples$BUGSoutput$mean$theta

saveRDS(object = theta, 
        file = "data/posterior-samples/posterior-predictive-bartlema-diagonal/gcm-all.rds")
