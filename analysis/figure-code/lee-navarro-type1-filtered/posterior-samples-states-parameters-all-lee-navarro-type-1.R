# Plot every participant's posterior samples for hidden states and parameters

source(file = "src/sampling-f/transform-data-chmm.R")
source(file = "src/plot-f/plot-trial-trial.R")

lee_navarro <- transform_data_chmm(
  directory_data = "data/csv-files/lee-navarro-2002-type1-filtered.csv",
  directory_features = "data/stimulus-features/lee-navarro-features.csv")

lee_long <- readr::read_csv(
  file = "data/csv-files/lee-navarro-2002-type1-filtered.csv")

model_adequacy <- readRDS(
  file = "data/posterior-samples/posterior-adequacy/lee-navarro-type1-filtered-chmm.rds")

samples <- readRDS(file = paste(c("data/posterior-samples/model-parameters/",
                                  "lee-navarro-type1-filtered-chmm.rds"),
                                collapse = ""))

grDevices::cairo_pdf(
  file = "figures/lee-navarro-type-1-filtered/parameters-states-all-lee-navarro.pdf",
  width = 8.3, height = 10.5, onefile = TRUE)

par(oma = c(3,3,10,2),
    mai = c(0.7,0.5,0,0),
    yaxs = "i", 
    xaxs = "i")

participants <- unique(lee_long$id)

for(pp in 1:length(samples$step_size)){
  layout(rbind(c(1, 1, 1, 1),
               c(1, 1, 1, 1),
               c(1, 1, 1, 1),
               c(1, 1, 1, 1),
               c(2, 3, 4, 5),
               c(2, 3, 4, 5),
               c(2, 3, 4, 5)))
  
  prop_c <- round(x = mean(x = subset(x = lee_long,
                                      subset = id == participants[pp])$correct), 
                  digits = 3)
  
  adequacy <- round(mean(model_adequacy[pp, ], na.rm = TRUE), 3)
  
  trial_trial_participant(data = lee_navarro, posterior_add = TRUE, 
                          posteriors = samples,
                          participant_id = pp, width = 0.6, height = 0.16,
                          category_color = c("#25272b", "#e1e4e2"),
                          border_color = "black", shade_stimulus = TRUE,
                          bar_width = 0.27, transparency_bars = FALSE, 
                          lwd_rect = 1.5)
  
  mtext(text = paste("Participant: ", pp), 
        side = 3, outer = TRUE, cex = 3, line = 6, at = 0.5)
  mtext(text = paste("Proportion of correct: ", prop_c), 
        side = 3, outer = TRUE, cex = 1.5, line = 2, at = 0.3)
  mtext(text = paste("Model adequacy: ", adequacy), 
        side = 3, outer = TRUE, cex = 1.5, line = 2, at = 0.8)
  
  box(bty = "l")
  axis(1, at = c(1, seq(10,lee_navarro$participant_t[pp], 10)), 
       padj = 0.1, tck = -0.02, cex.axis = 1.4)
  axis(2, las = 2, at = seq(0.16, dim(lee_navarro$response)[1], 1),
       labels = FALSE, tck = -0.01)
  axis(2, las = 2, at = seq(0.85, dim(lee_navarro$response)[1], 1),
       labels = FALSE, tck = -0.01)
  
  # positions <- seq(0.5, dim(lee_navarro$response)[1], 1)
  # label_code <- rep(x = c("\U25CF ","\U25A0", "\U25B2"), each = 3)
  # color_code <- c("black", NA, NA, NA, "black", NA, NA, NA, "black")
  # size_code <- rep(x = c(2,1.4,2.2), each = 3)
  # hadj_code <- c(0.22, NA, NA, NA, 0.199, NA, NA, NA, 0.37)
  # padj_code <- c(0.44, NA, NA, NA, 0.51, NA, NA, NA, 0.42)
  # for (i in 1:length(positions)) {
  #   axis(2, at = positions[i],
  #        labels = paste(label_code[i]),
  #        las = 2, tck = 0, 
  #        hadj = hadj_code[i], 
  #        cex.axis = size_code[i], 
  #        padj = padj_code[i], 
  #        col.axis = paste(color_code[i]))
  # }
  
  positions <- seq(0.5, dim(lee_navarro$response)[1], 1)
  label_code <- rep(x = c("\U25CF ","\U25A0", "\U25B2"), each = 3)
  color_code <- rep(x = c("#d72631", "#77c593", "#1868ae"), times = 3)
  size_code <- rep(x = c(2,1.5,2), each = 3)
  hadj_code <- rep(x = c(0.18,0.1,0.33), each = 3)
  for (i in 1:length(positions)) {
    axis(2, at = positions[i],
         labels = paste(label_code[i]),
         las = 2, tck = 0, hadj = hadj_code[i], 
         cex.axis = size_code[i], padj = 0.45, 
         col.axis = paste(color_code[i]))
  }
  
  mtext(text = "Trial", side = 1, cex = 1.6, line = 3)
  mtext(text = "Posterior Mean Category", side = 2, cex = 1.6, line = 3)
  
  hist(samples$posterior_samples$gamma[, pp], xlim = c(0, 1), axes = FALSE,
       ann = FALSE, border = "#FCF6F5", col = "#30626d", 
       breaks = seq(0, 1, 0.05))
  
  box(bty = "l")
  axis(side = 1, at = c(0,0.2,0.5,0.8,1), cex.axis = 1.4,
       labels = (c("0", 0.2, 0.5, 0.8, "1")), padj = -0.2)
  mtext(text = expression(gamma), side = 1, line = 3, cex = 1.6)
  
  hist(samples$posterior_samples$epsilon[, pp], xlim = c(0,0.04), axes = FALSE,
       ann = FALSE, border = "#FCF6F5", col = "#723f75", 
       breaks = seq(0,0.05,0.0025))
  box(bty = "l")
  axis(side = 1, at = seq(0,0.04,0.02), cex.axis = 1.4,
       labels = (c("0", seq(0.02,0.04,0.02))), padj = -0.2)
  mtext(text = expression(epsilon), side = 1, line = 3, cex = 1.6)
  
  hist(samples$posterior_samples$alpha[, pp], xlim = c(1, 12), axes = FALSE,
       ann = FALSE, border = "#FCF6F5", col = "#30626d",
       breaks = seq(1,12,0.5))
  box(bty = "l")
  axis(side = 1, at = c(1, 4, 8, 12), cex.axis = 1.4, padj = -0.2)
  mtext(text = expression(beta[0]), side = 1, line = 3, cex = 1.6)
  
  hist(samples$posterior_samples$beta[, pp], xlim = c(1, 12), axes = FALSE,
       ann = FALSE, border = "#FCF6F5", col = "#723f75",
       breaks = seq(1,12,0.5))
  box(bty = "l")
  axis(side = 1, at = c(1, 4, 8, 12), cex.axis = 1.4, padj = -0.2)
  mtext(text = expression(beta[1]), side = 1, line = 3, cex = 1.6)
}
dev.off()
