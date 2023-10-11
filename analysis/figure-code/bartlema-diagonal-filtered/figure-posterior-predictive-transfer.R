# Plot posterior predictive distribution by transfer trial
pdf(file = "figures/bartlema-posterior-predictive-transfers.pdf",
  width = 8, height = 2.5)

bart <- readr::read_csv(file = "data/csv-files/bartlema-diagonal.csv")

par(mai = c(0,0,0,0),
  oma = c(0.1,2,0.1,2))

layout(t(seq(1,4)))

transfers <- c(1,3,5,7)

for (i in 1:4) {
  plot_pp_transfer(data = bart, 
                 path_posterior = 
                   paste(c("data/posterior-samples/posterior-predictive-bartlema-diagonal/chmm-transfer-",
                           transfers[i],".rds"), collapse = ""),
                 path_comparison = 
                   paste(c("data/posterior-samples/posterior-predictive-bartlema-diagonal/gcm-transfer-",
                           transfers[i],".rds"), collapse = ""),
                 participant_id = 27, transfer_id = transfers[i],
                 inner_height = 0.14, relative_pos = 0.08,
                 outer_lwd = 3)
}

dev.off()
