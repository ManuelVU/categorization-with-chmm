# Code to plot posterior mean state and participant's responses

source(file = "src/sampling-f/transform-data-chmm.R")
source(file = "src/plot-f/plot-trial-trial.R")

# load data
lee <- readr::read_csv(
  file = "data/csv-files/lee-navarro-2002-type4-filtered.csv")

# participants to remove from the analysis
participants_id <- unique(lee$id)[-c(which(unique(lee$id) == 6),
                                     which(unique(lee$id) == 22))]

# filter original data
lee_filt <- lee |>
  subset(subset = id %in% participants_id)

# Assign labels to participants according to their number of trials
participant_labels <- LETTERS[seq(from = 1, to = length(unique(lee_filt$id)))]
participant_order <- lee_filt |> 
  dplyr::select(id, trial_condition) |> 
  dplyr::group_by(id) |> 
  dplyr::summarise("n_trials" = max(trial_condition)) |> 
  dplyr::pull(n_trials) |> 
  order(decreasing = FALSE)

lee_navarro <- transform_data_chmm(
  directory_data = "data/csv-files/lee-navarro-2002-type4-filtered.csv",
  directory_features = "data/stimulus-features/lee-navarro-features.csv")

samples <- readRDS(file = paste(c("data/posterior-samples/model-parameters/",
                                  "lee-navarro-type4-filtered-chmm.rds"),
                                collapse = ""))

grDevices::cairo_pdf(
  file = "figures/lee-navarro-type-4-filtered/trial-trial-plot-pa-lee-navarro.pdf",
  width = 8.3, height = 4.5)

par(oma = c(2.5,2.5,1,0.5),
    mai = c(0.1,0.2,0.1,0),
    yaxs = "i", 
    xaxs = "i")

pp <- 3

trial_trial_participant(data = lee_navarro, posterior_add = TRUE, 
                        posteriors = samples,
                        participant_id = pp, width = 0.75, height = 0.33,
                        category_color = c("#25272b", "white"),
                        border_color = "black", shade_stimulus = TRUE,
                        bar_width = 0.27, transparency_bars = FALSE, 
                        lwd_rect = 1.5)
box(bty = "l")

mtext(text = paste(
  "Participant: ", 
  participant_labels[which(participant_t[order(participant_t)] == 49)]), 
      side = 3, outer = TRUE, cex = 1.4, line = -0.5)

axis(1, padj = -0.5, tck = -0.02, cex.axis = 1.2,
     at = c(1, seq(10,lee_navarro$participant_t[pp], 10), 
            lee_navarro$participant_t[pp]))
axis(2, las = 2, at = seq(0.16, dim(lee_navarro$response)[1], 1),
     labels = FALSE, tck = -0.01)
axis(2, las = 2, at = seq(0.85, dim(lee_navarro$response)[1], 1),
     labels = FALSE, tck = -0.01)

positions <- seq(0.5, dim(lee_navarro$response)[1], 1)
label_code <- rep(x = c("\U25CF ","\U25A0", "\U25B2"), each = 3)
color_code <- c("#25272b", NA, NA, NA, "#25272b", NA, NA, NA, "#25272b")
size_code <- rep(x = c(2,1.4,2.2), each = 3)
hadj_code <- c(0.24, NA, NA, NA, 0.25, NA, NA, NA, 0.39)
padj_code <- c(0.41, NA, NA, NA, 0.52, NA, NA, NA, 0.42)
for (i in 1:length(positions)) {
  axis(2, at = positions[i],
       labels = paste(label_code[i]),
       las = 2, tck = 0, 
       hadj = hadj_code[i], 
       cex.axis = size_code[i], 
       padj = padj_code[i], 
       col.axis = paste(color_code[i]))
}

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
mtext(text = "Trial", side = 1, cex = 1.4, line = 2)
mtext(text = "Stimuli", side = 2, cex = 1.4, line = 2)
dev.off()

grDevices::cairo_pdf(
  file = "figures/lee-navarro-type-4-filtered/trial-trial-plot-ph-lee-navarro.pdf",
  width = 8.3, height = 4.5)

par(oma = c(2.5,2.5,1,0.5),
    mai = c(0.1,0.2,0.1,0),
    yaxs = "i", 
    xaxs = "i")

pp <- 6

trial_trial_participant(data = lee_navarro, 
                        posterior_add = TRUE, 
                        posteriors = samples,
                        participant_id = pp, 
                        width = 0.75, 
                        height = 0.33,
                        category_color = c("#25272b", "white"),
                        border_color = "black", 
                        shade_stimulus = TRUE,
                        bar_width = 0.27, 
                        transparency_bars = FALSE, 
                        lwd_rect = 1.5)

mtext(text = paste(
  "Participant: ",
  participant_labels[which(participant_t[order(participant_t)] == 77)]), 
      side = 3, outer = TRUE, cex = 1.4, line = -0.5)

box(bty = "l")
axis(1, padj = -0.5, tck = -0.02, cex.axis = 1.2,
     at = c(1, seq(10,lee_navarro$participant_t[pp], 10), 
            lee_navarro$participant_t[pp]))
axis(2, las = 2, at = seq(0.16, dim(lee_navarro$response)[1], 1),
     labels = FALSE, tck = -0.01)
axis(2, las = 2, at = seq(0.85, dim(lee_navarro$response)[1], 1),
     labels = FALSE, tck = -0.01)

positions <- seq(0.5, dim(lee_navarro$response)[1], 1)
label_code <- rep(x = c("\U25CF ","\U25A0", "\U25B2"), each = 3)
color_code <- c("#25272b", NA, NA, NA, "#25272b", NA, NA, NA, "#25272b")
size_code <- rep(x = c(2,1.4,2.2), each = 3)
hadj_code <- c(0.24, NA, NA, NA, 0.25, NA, NA, NA, 0.39)
padj_code <- c(0.41, NA, NA, NA, 0.52, NA, NA, NA, 0.42)
for (i in 1:length(positions)) {
  axis(2, at = positions[i],
       labels = paste(label_code[i]),
       las = 2, tck = 0, 
       hadj = hadj_code[i], 
       cex.axis = size_code[i], 
       padj = padj_code[i], 
       col.axis = paste(color_code[i]))
}

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

mtext(text = "Trial", side = 1, cex = 1.4, line = 2)
mtext(text = "Stimuli", side = 2, cex = 1.4, line = 2)
dev.off()
