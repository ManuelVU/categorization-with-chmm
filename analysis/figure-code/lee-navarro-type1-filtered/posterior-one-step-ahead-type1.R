################################################################################
## Posterior one step ahead adequacy plot                                     ##
################################################################################

# clean environment including hidden objects
rm(list = ls(all.names = TRUE))

# free RAM usage
gc()

# load moving average function 
source(file = "src/summary-f/moving-average.R")

# load data
lee <- readr::read_csv(
  file = "data/csv-files/lee-navarro-2002-type1-filtered.csv")

# participants to remove from the analysis
participants_id <- unique(lee$id)[-c(which(unique(lee$id) == 6),
                                     which(unique(lee$id) == 22))]

# filter original data
lee_filt <- lee |>
  subset(subset = id %in% participants_id)

# load posterior one step ahead samples for the single rate model
os_ahead_stick <- readRDS(
  file = paste(c("data/posterior-samples/posterior-adequacy/",
                 "one-step-ahead-type1-chmm-stick.rds"), 
               collapse = ""))

# load posterior one step ahead samples for the single rate and interaction 
# model
os_ahead_interaction <- readRDS(
  file = paste(c("data/posterior-samples/posterior-adequacy/",
                 "one-step-ahead-type1-chmm-stick-interaction.rds"), 
               collapse = ""))

# removing participants from analysis due to incorrect coding of responses
os_ahead_stick <- os_ahead_stick[-c(which(unique(lee$id) == 6),
                                    which(unique(lee$id) == 22)), ]

os_ahead_interaction <- os_ahead_interaction[-c(which(unique(lee$id) == 6),
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
participant_t <- rowSums(1 - is.na(os_ahead_stick))

plot(0,0, type = "n", ylim = c(-1,1), xlim = c(0, max(participant_t) + 5),
     axes = FALSE, ann = FALSE)
box(bty = "l")
abline(h = 0, lwd = 1.5, lty = 2)

for (i in 1:dim(os_ahead_stick)[1]) {
  p <- os_ahead_interaction[i, ]
  q <- os_ahead_stick[i,]
  
  logit_part_i <- moving_average(data = log(x = (p / (1-p)) / (q / (1-q))),
    window_size = 5)
  
  lines(x = logit_part_i, lwd = 1.5, col = "#8595b555", type = "o", pch = 16,
        cex = 0.5)
}

axis(side = 2, at = c(-0.5, 0, 0.5),
     labels = c("sitickiness", "", "similarity"))


