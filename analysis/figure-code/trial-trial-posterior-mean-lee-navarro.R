# Code to plot posterior mean state and participant's responses
grDevices::cairo_pdf(file = "figures/lee-navarro-trial-trial-participant-posterior.pdf",
                     width = 8.3, height = 3.8)
par(oma = c(1.6,2.1,0.1,0.1),
    mai = c(0.1,0.1,0.1,0.1),
    xaxs = "i", yaxs = "i")

lee_navarro <- transform_data_chmm(
  directory_data = "data/csv-files/lee-navarro-2002-type4-filtered.csv",
  directory_features = "data/stimulus-features/lee-navarro-features.csv")

samples <- readRDS(file = "data/posterior-samples/lee-navarro-2002-type4-posterior-samples-2.rds")

source(file = "src/plot-f/trial-trial-plot.R")

pp <- 3

trial_trial_participant(data = lee_navarro, posterior_add = TRUE, 
                        posteriors = samples,
                        participant_id = pp, width = 0.7, height = 0.23,
                        category_color = c("#30626d", "#723f75"),
                        border_color = "#FCF6F5FF", shade_stimulus = TRUE)
box(bty = "l")
axis(1, at = c(1, seq(10,lee_navarro$participant_t[pp], 10)), padj = -1.3, tck = -0.02)
axis(2, las = 2, at = seq(0.16, dim(lee_navarro$response)[1], 1),
     labels = FALSE, tck = -0.01)
axis(2, las = 2, at = seq(0.85, dim(lee_navarro$response)[1], 1),
     labels = FALSE, tck = -0.01)

positions <- seq(0.5, dim(lee_navarro$response)[1], 1)
label_code <- rep(x = c("\U25CF ","\U25A0", "\U25B2"), each = 3)
color_code <- rep(x = c("#d72631", "#77c593", "#1868ae"), times = 3)
size_code <- rep(x = c(1.5,1,1.5), each = 3)
hadj_code <- rep(x = c(0.18,0.1,0.33), each = 3)
for (i in 1:length(positions)) {
  axis(2, at = positions[i],
       labels = paste(label_code[i]),
       las = 2, tck = 0, hadj = hadj_code[i], 
       cex.axis = size_code[i], padj = 0.45, 
       col.axis = paste(color_code[i]))
}
mtext(text = "Trial", side = 1, outer = TRUE, cex = 1, line = 0.6)
mtext(text = "Posterior Mean Category", side = 2, outer = TRUE,
      cex = 1, line = 1)
dev.off()