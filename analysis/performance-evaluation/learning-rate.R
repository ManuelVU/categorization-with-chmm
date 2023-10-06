# Evaluating learning performance in bartlema et al criss-cross and diagonal 
# category structures.

library(R2jags)

bart <- readr::read_csv(file = "data/csv-files/bartlema-diagonal.csv")

window_size <- 15

trials <- length(bart$trial[bart$id == 1 & bart$condition_char == "learn"])

n_participants <- length(unique(bart$id))

roll_mean_correct <- matrix(data = NA, 
                            nrow = n_participants, 
                            ncol = trials - window_size + 1)

for (i in 1:n_participants) {
  roll_mean_correct[i, ] <- zoo::rollmean(subset(x = bart, 
                                          subset = id == i & 
                                            condition_char == "learn")$correct, 
                                          k = window_size)
}

jags_data <- list("trials" = dim(roll_mean_correct)[2], 
                  "participants" = dim(roll_mean_correct)[1], 
                  "y" = roll_mean_correct)

model <- "model{
  for(n in 1:participants){
  # Prior ddistributions
  beta_0[n] ~ dnorm(0, 1)
  
  beta_1[n] ~ dnorm(0, 1)T(0, )
  
  tau[n] ~ dgamma(0.001, 0.001)
  
    for(t in 1:trials){
      y[n, t] ~ dnorm(beta_0[n] + beta_1[n] * t, tau[n])
      
    }
  }
  
}"

parameters <- c("beta_0", "beta_1")

samples <- jags(data = jags_data, parameters.to.save = parameters,
                model.file = textConnection(model), n.chains = 5, 
                n.iter = 10000, n.burnin = 9000, n.thin = 1)

bf <- c()
p_75 <- c()

for (i in 1:n_participants) {
  dx <- logspline::logspline(x = samples$BUGSoutput$sims.list$beta_1[, i],
                             lbound = 0,
                             error.action = 2)
  
  dx_0 <- logspline::dlogspline(q = 0, fit = dx)
  
  bf[i] <- truncnorm::dtruncnorm(x = 0, a = 0, mean = 0, sd = 1) / dx_0
  
  p_75[i] <- mean((samples$BUGSoutput$sims.list$beta_0[, i] - 0.7) > 0)
   
}

participants_to_keep_diagonal <- sort(x = unique(which(bf > 10), which(p_75 > 0.5)))

#### Criss-cross ----
library(R2jags)

bart <- readr::read_csv(file = "data/csv-files/bartlema-crisscross.csv")

window_size <- 15

trials <- length(bart$trial[bart$id == 1 & bart$condition_char == "learn"])

n_participants <- length(unique(bart$id))

roll_mean_correct <- matrix(data = NA, 
                            nrow = n_participants, 
                            ncol = trials - window_size + 1)

for (i in 1:n_participants) {
  roll_mean_correct[i, ] <- zoo::rollmean(subset(x = bart, 
                                                 subset = id == i & 
                                                   condition_char == "learn")$correct, 
                                          k = window_size)
}

jags_data <- list("trials" = dim(roll_mean_correct)[2], 
                  "participants" = dim(roll_mean_correct)[1], 
                  "y" = roll_mean_correct)

model <- "model{
  for(n in 1:participants){
  # Prior ddistributions
  beta_0[n] ~ dnorm(0, 1)
  
  beta_1[n] ~ dnorm(0, 1)T(0, )
  
  tau[n] ~ dgamma(0.001, 0.001)
  
    for(t in 1:trials){
      y[n, t] ~ dnorm(beta_0[n] + beta_1[n] * t, tau[n])
      
    }
  }
  
}"

parameters <- c("beta_0", "beta_1")

samples <- jags(data = jags_data, parameters.to.save = parameters,
                model.file = textConnection(model), n.chains = 5, 
                n.iter = 10000, n.burnin = 9000, n.thin = 1)

bf <- c()
p_75 <- c()

for (i in 1:n_participants) {
  dx <- logspline::logspline(x = samples$BUGSoutput$sims.list$beta_1[, i],
                             lbound = 0,
                             error.action = 2)
  
  dx_0 <- logspline::dlogspline(q = 0, fit = dx)
  
  bf[i] <- truncnorm::dtruncnorm(x = 0, a = 0, mean = 0, sd = 1) / dx_0
  
  p_75[i] <- mean((samples$BUGSoutput$sims.list$beta_0[, i] - 0.7) > 0)
  
}

participants_to_keep_criss <- sort(x = unique(which(bf > 10), which(p_75 > 0.5)))

