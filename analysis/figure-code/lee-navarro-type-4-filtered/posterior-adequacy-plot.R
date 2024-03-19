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

# participants to remove from the analysis
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

part_color<- c("#d72631", "#77c593", "#1868ae")

plot_posterior_adequacy(data = lee_filt,
                        samples_y = adequacy,
                        category_color = c("#30626d", "#723f75"),
                        order = "decreasing",
                        fill_color = NA,
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
                                 window_size = 5)
  
  lines(x = logit_part_i, lwd = 1.5, col = "#8595b555", type = "o", pch = 16,
        cex = 0.5)
  
}

highlight_p <- c(which(participant_t == 49),
                 which(participant_t == 68)[1],
                 which(participant_t == 77))

count <- 0

for(i in highlight_p) {
  count <- count + 1
  
  logit_part_i <- 
    moving_average(data = log(adequacy[i, 1:participant_t[i]] / 
                                (1 - adequacy[i, 1:participant_t[i]])), 
                   window_size = 5)
  
  lines(x = logit_part_i, lwd = 1.5, type = "o", pch = 16, cex = 0.5,
        col = part_color[count])
}

text(x = c(50, 69, 78), 
     y = log(0.992 / (1 - 0.992)), 
     labels = participant_labels[c(1, 4, 8)], 
     cex = 1.3, col = part_color)

axis(side = 1, at = c(1, seq(from = 10, to = 100, 10)), 
     padj = - 1.7, tck = -0.02, cex.axis = 0.85)

axis(side = 2, las = 2, at = log(c(0.5, 0.9, 0.99) / (1 - c(0.5, 0.9, 0.99))),
     labels = c("0.50", "0.90", "0.99"),
     cex.axis = 0.85, hadj = 0.55, tck = -0.02)

mtext(text = "Probability of agreement", side = 2, line = 1.8, cex = 1.25)

mtext(text = "Trial", side = 1, outer = TRUE, cex = 1.25, line = 1.5)
dev.off()
