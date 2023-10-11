# Figure posterior predictive accuracy by model

# Load posterior predictive accuracy by model
pp_acc <- readRDS(file = "data/posterior-samples/posterior-predictive-bartlema-diagonal/accuracy.rds")

# Load plotting function
source(file = "src/plot-f/horizontal-hist.R")

pdf(file = "figures/bartlema-posterior-predictive-accuracy.pdf", width = 8, 
    height = 3)

par(mai = c(0,0,0,0),
    oma = c(2.5,3,0.5,0.5))

# Create an empty ploting region
plot(x = 0, y = 0, ann = FALSE, type = "n", axes = FALSE, xlim = c(0.5, 5),
     ylim = c(0, 1))

# create horizontal histograms
for (i in 1:4) {
  horizontal_hist(data_x1 = pp_acc$chmm[, i, ], data_x2 = pp_acc$gcm[, i, ],
                  center = i, height = seq(0,1,0.03))  
}
abline(v = c(1, 2, 3, 4), lty = 2, lwd = 1)
box(bty = "l")
axis(side = 1, at = seq(1,4), cex.axis = 1, padj = -0.9)
axis(side = 2, at = seq(0,1, 0.2), labels = c("0", seq(0.2,0.8,0.2), "1"),
     cex.axis = 1, hadj = 0.68, las = 2)
mtext(text = "Transfer phase", side = 1, line = 1.4, cex = 1.3)
mtext(text = "Posterior accuracy", side = 2, line = 1.9, cex = 1.3)
legend("topright", legend = c("CHMM", "GCM"), pch = 15, 
       col = c("#30626d", "#723f75"), bty = "n", cex = 1.3)

dev.off()


#### Figure mean posterior accuracy ----
pp_acc <- readRDS(file = "data/posterior-samples/posterior-predictive-bartlema-diagonal/accuracy.rds")

# Load plotting function
source(file = "src/plot-f/horizontal-hist.R")

pdf(file = "figures/bartlema-mean-posterior-predictive-accuracy.pdf", width = 8, 
    height = 3)

par(mai = c(0,0,0,0),
    oma = c(2.5,3,0.5,0.5))

pp_acc$mean_chmm <- apply(X = pp_acc$chmm, MARGIN = c(1, 2), FUN = mean)

pp_acc$mean_gcm <- apply(X = pp_acc$gcm, MARGIN = c(1, 2), FUN = mean)

# Create an empty plotting region
plot(x = 0, y = 0, ann = FALSE, type = "n", axes = FALSE, xlim = c(0.5, 5),
     ylim = c(0, 1))

# create horizontal histograms
for (i in 1:4) {
  horizontal_hist(data_x1 = pp_acc$mean_chmm[, i], data_x2 = pp_acc$mean_gcm[, i],
                  center = i, height = seq(0,1,0.025))  
}
abline(v = c(1, 2, 3, 4), lty = 2, lwd = 1)
box(bty = "l")
axis(side = 1, at = seq(1,4), cex.axis = 1, padj = -0.9)
axis(side = 2, at = seq(0,1, 0.2), labels = c("0", seq(0.2,0.8,0.2), "1"),
     cex.axis = 1, hadj = 0.68, las = 2)
mtext(text = "Transfer phase", side = 1, line = 1.4, cex = 1.3)
mtext(text = "Mean posterior accuracy", side = 2, line = 1.9, cex = 1.3)
legend("topright", legend = c("CHMM", "GCM"), pch = 15, 
       col = c("#30626d", "#723f75"), bty = "n", cex = 1.3)

dev.off()
