################################################################################
## Posterior posterior adequacy plots                                         ##
################################################################################

# clean environment including hidden objects
rm(list = ls(all.names = TRUE))

# free RAM usage
gc()

# load moving average function 
source(file = "src/summary-f/moving-average.R")

# load posterior adequacy plotting function
source(file = "src/plot-f/plot-posterior-adequacy.R")

# load data
lee <- readr::read_csv(
  file = "data/csv-files/lee-navarro-2002-type4-filtered.csv")

# participants to remove from the analisis
participants_id <- unique(lee$id)[-c(which(unique(lee$id) == 6),
                                     which(unique(lee$id) == 22))]

# filter original data
lee_filt <- lee |>
  subset(subset = id %in% participants_id)

# load posterior adequacy samples
adequacy <- readRDS(file = paste(c("data/posterior-samples/posterior-adequacy/",
                                   "lee-navarro-type4-filtered-chmm.rds"), 
                                 collapse = ""))

adequacy <- adequacy[-c(which(unique(lee$id) == 6),
                        which(unique(lee$id) == 22)), ]

# Assign labels to participants according to their number of trials
participant_labels <- LETTERS[seq(from = 1, to = length(unique(lee_filt$id)))]
participant_order <- lee_filt |> 
  dplyr::select(id, trial_condition) |> 
  dplyr::group_by(id) |> 
  dplyr::summarise("n_trials" = max(trial_condition)) |> 
  dplyr::pull(n_trials) |> 
  order(decreasing = FALSE)

# number of trials per participant in adequacy matrix
participant_t <- rowSums(1 - is.na(adequacy))

pdf(file = "figures/lee-navarro-type-4-filtered/posterior-adequacy.pdf",
    width = 8.5, height = 3.2)

layout(t(seq(1,2)))

par(oma = c(2.5,0,0.5,0.5),
    mai = c(0,0.55,0,0),
    xaxs = "i")

plot_posterior_adequacy(data = lee_filt,
                        samples_y = adequacy,
                        order = "decreasing",
                        category_color = c("#30626d", "#723f75"),
                        width = 0.67,
                        height = 0.6, 
                        increase = 5)
box(bty = "l")

axis(side = 1, at = c(1, seq(from = 10, to = 100, 10)), 
     padj = - 1.7, tck = -0.02, cex.axis = 0.85)

axis(side = 2, las = 2, 
     at = seq(from = 1, to = length(x = unique(x = lee_filt$id))),
     labels = participant_labels, tck = -0.02, hadj = 0)

mtext(text = "Participant", side = 2, line = 1.5, cex = 1.25)


plot(0,0, type = "n", ylim = c(0,5), xlim = c(0, max(participant_t) + 5),
     axes = FALSE, ann = FALSE)
box(bty = "l")

for (i in 1:dim(adequacy)[1]) {
  logit_part_i <- moving_average(data = log(adequacy[i, 1:participant_t[i]] / 
                                        (1 - adequacy[i, 1:participant_t[i]])), 
                                 window_size = 10)
  
  lines(x = logit_part_i, lwd = 1.5, col = "#80858877", type = "o", pch = 16,
        cex = 0.5)
  
}

highlight_p <- c(which(participant_t == 49),
                 which(participant_t == 77))

for(i in highlight_p) {
  logit_part_i <- 
    moving_average(data = log(adequacy[i, 1:participant_t[i]] / 
                                (1 - adequacy[i, 1:participant_t[i]])), 
                   window_size = 10)
  
  lines(x = logit_part_i, lwd = 1.5, type = "o", pch = 16, cex = 0.5,
        col = "#25272b")
}

text(x = c(50, 78), 
     y = log(0.992 / (1 - 0.992)), 
     labels = participant_labels[c(1,8)], 
     cex = 1.3)

axis(side = 1, at = c(1, seq(from = 10, to = 100, 10)), 
     padj = - 1.7, tck = -0.02, cex.axis = 0.85)

axis(side = 2, las = 2, at = seq(from = 0, to = 5),
     labels = 100 * round(x = exp(seq(from = 0, to = 5)) / 
                              (1 + exp(seq(from = 0, to = 5))), 
                          digits = 2),
     cex.axis = 1, hadj = 0.25, tck = -0.02)

mtext(text = "Proportion of agreement", side = 2, line = 1.5, cex = 1.25)

mtext(text = "Trial", side = 1, outer = TRUE, cex = 1.25, line = 1.5)
dev.off()
