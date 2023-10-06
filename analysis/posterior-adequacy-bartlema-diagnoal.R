# Figure posterior adequacy by model

# Load posterior adequacy by model
pp_add <- readRDS(file = "data/posterior-samples/posterior-predictive-bartlema-diagonal/adequacy.rds")

# Load plotting function
source(file = "src/plot-f/horizontal-hist.R")

#### Figure: Posterior adequacy ----
pdf(file = "figures/bartlema-posterior-adequacy-trial-stimulus.pdf", width = 8, 
    height = 3)

par(mai = c(0,0,0,0),
    oma = c(2.5,3,0.5,0.5))

# Create an empty ploting region
plot(x = 0, y = 0, ann = FALSE, type = "n", axes = FALSE, xlim = c(0.5, 1.5),
     ylim = c(0, 1))

# create horizontal histograms
horizontal_hist(data_x1 = pp_add$chmm[, , ], data_x2 = pp_add$gcm[, , ],
                  center = 1, height = seq(0,1,0.03))
abline(v = 1, lty = 2, lwd = 1)
box(bty = "l")
# axis(side = 1, at = seq(1,4), cex.axis = 1, padj = -0.9)
axis(side = 2, at = seq(0,1, 0.2), labels = c("0", seq(0.2,0.8,0.2), "1"),
     cex.axis = 1, hadj = 0.68, las = 2)
# mtext(text = "Transfer phase", side = 1, line = 1.4, cex = 1.3)
mtext(text = "Posterior adequacy", side = 2, line = 1.9, cex = 1.3)
legend("topright", legend = c("CHMM", "GCM"), pch = 15, 
       col = c("#30626d", "#723f75"), bty = "n", cex = 1.3)

dev.off()

#### Figure: mean posterior adequacy across trials ----
pdf(file = "figures/bartlema-posterior-adequacy-stimulus.pdf", width = 8, 
    height = 3)

par(mai = c(0,0,0,0),
    oma = c(2.5,3,0.5,0.5))

# Create an empty ploting region
plot(x = 0, y = 0, ann = FALSE, type = "n", axes = FALSE, xlim = c(0.5, 1.5),
     ylim = c(0, 1))

# create horizontal histograms
horizontal_hist(data_x1 = pp_add$chmm_mean_trial[, ], 
                data_x2 = pp_add$gcm_mean_trial[, ],
                center = 1, height = seq(0,1,0.03))
abline(v = 1, lty = 2, lwd = 1)
box(bty = "l")
# axis(side = 1, at = seq(1,4), cex.axis = 1, padj = -0.9)
axis(side = 2, at = seq(0,1, 0.2), labels = c("0", seq(0.2,0.8,0.2), "1"),
     cex.axis = 1, hadj = 0.68, las = 2)
# mtext(text = "Transfer phase", side = 1, line = 1.4, cex = 1.3)
mtext(text = "Mean posterior accuracy", side = 2, line = 1.9, cex = 1.3)
legend("topright", legend = c("CHMM", "GCM"), pch = 15, 
       col = c("#30626d", "#723f75"), bty = "n", cex = 1.3)

dev.off()

#### Figure: mean posterior adequacy ----
pdf(file = "figures/bartlema-posterior-adequacy.pdf", width = 8, 
    height = 3)

par(mai = c(0,0,0,0),
    oma = c(2.5,3,0.5,0.5))

# Create an empty ploting region
plot(x = 0, y = 0, ann = FALSE, type = "n", axes = FALSE, xlim = c(0.5, 1.5),
     ylim = c(0, 1))

# create horizontal histograms
horizontal_hist(data_x1 = pp_add$chmm_mean_trial_stimulus, 
                data_x2 = pp_add$gcm_mean_trial_stimulus,
                center = 1, height = seq(0,1,0.02))
abline(v = 1, lty = 2, lwd = 1)
box(bty = "l")
# axis(side = 1, at = seq(1,4), cex.axis = 1, padj = -0.9)
axis(side = 2, at = seq(0,1, 0.2), labels = c("0", seq(0.2,0.8,0.2), "1"),
     cex.axis = 1, hadj = 0.68, las = 2)
# mtext(text = "Transfer phase", side = 1, line = 1.4, cex = 1.3)
mtext(text = "Mean posterior adequacy", side = 2, line = 1.9, cex = 1.3)
legend("topright", legend = c("CHMM", "GCM"), pch = 15, 
       col = c("#30626d", "#723f75"), bty = "n", cex = 1.3)

dev.off()

#### Figure: posterior adequacy training vs transfer ----
pdf(file = "figures/bartlema-posterior-adequacy-training-vs-transfer.pdf", width = 8, 
    height = 3)

par(mai = c(0,0,0,0),
    oma = c(2.5,3,0.5,0.5))

training <- c(2, 3, 5, 7, 10, 12, 14, 15)

# Create an empty ploting region
plot(x = 0, y = 0, ann = FALSE, type = "n", axes = FALSE, xlim = c(0.5, 2.5),
     ylim = c(0, 1))

# create horizontal histograms
horizontal_hist(data_x1 = pp_add$chmm[, -seq(1, 216), training], 
                data_x2 = pp_add$gcm[, -seq(1, 216), training],
                center = 1, height = seq(0,1,0.03))

horizontal_hist(data_x1 = pp_add$chmm[, -seq(1, 216), -training], 
                data_x2 = pp_add$gcm[, -seq(1, 216), -training],
                center = 2, height = seq(0,1,0.03))
abline(v = 1, lty = 2, lwd = 1)
box(bty = "l")
# axis(side = 1, at = seq(1,4), cex.axis = 1, padj = -0.9)
axis(side = 2, at = seq(0,1, 0.2), labels = c("0", seq(0.2,0.8,0.2), "1"),
     cex.axis = 1, hadj = 0.68, las = 2)
# mtext(text = "Transfer phase", side = 1, line = 1.4, cex = 1.3)
mtext(text = "Posterior adequacy", side = 2, line = 1.9, cex = 1.3)
legend("topright", legend = c("CHMM", "GCM"), pch = 15, 
       col = c("#30626d", "#723f75"), bty = "n", cex = 1.3)

dev.off()

#### Figure: mean posterior adequacy training vs transfer collapse trials ----
pdf(file = "figures/bartlema-posterior-adequacy-training-vs-transfer-trials.pdf", width = 8, 
    height = 3)

par(mai = c(0,0,0,0),
    oma = c(2.5,3,0.5,0.5))

training <- c(2, 3, 5, 7, 10, 12, 14, 15)

# Create an empty ploting region
plot(x = 0, y = 0, ann = FALSE, type = "n", axes = FALSE, xlim = c(0.5, 2.5),
     ylim = c(0, 1))

# create horizontal histograms
horizontal_hist(data_x1 = pp_add$chmm_mean_trial[, training], 
                data_x2 = pp_add$gcm_mean_trial[, training],
                center = 1, height = seq(0,1,0.03))

horizontal_hist(data_x1 = pp_add$chmm_mean_trial[, -training], 
                data_x2 = pp_add$gcm_mean_trial[, -training],
                center = 2, height = seq(0,1,0.03))
abline(v = 1, lty = 2, lwd = 1)
box(bty = "l")
axis(side = 1, at = seq(1,2), labels = c("Training", "Transfer"), 
     cex.axis = 1, padj = -0.9)
axis(side = 2, at = seq(0,1, 0.2), labels = c("0", seq(0.2,0.8,0.2), "1"),
     cex.axis = 1, hadj = 0.68, las = 2)
# mtext(text = "Transfer phase", side = 1, line = 1.4, cex = 1.3)
mtext(text = "Posterior adequacy", side = 2, line = 1.9, cex = 1.3)
legend("topright", legend = c("CHMM", "GCM"), pch = 15, 
       col = c("#30626d", "#723f75"), bty = "n", cex = 1.3)

dev.off()
