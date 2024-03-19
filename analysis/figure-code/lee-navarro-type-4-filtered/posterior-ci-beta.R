################################################################################
## Figure, posterior mean and CI for the stickiness parameters                ##
################################################################################

# clean environment including hidden objects
rm(list = ls(all.names = TRUE))

# free RAM usage
gc()

# load data
lee <- readr::read_csv(
  file = "data/csv-files/lee-navarro-2002-type4-filtered.csv")

# participants to remove from the analysis
participants_id <- unique(lee$id)[-c(which(unique(lee$id) == 6),
                                     which(unique(lee$id) == 22))]

# filter original data
lee_filt <- lee |>
  subset(subset = id %in% participants_id)


# load posterior samples
samples <- readRDS(file = paste(c("data/posterior-samples/model-parameters/",
                                  "lee-navarro-type4-filtered-chmm.rds"),
                                collapse = ""))

# store posterior samples associated with category 0 in beta_0
beta_0 <- samples$posterior_samples$alpha[, -c(which(unique(lee$id) == 6),
                                               which(unique(lee$id) == 22))]

# store posterior samples associated with category 1 in beta_1
beta_1 <- samples$posterior_samples$beta[, -c(which(unique(lee$id) == 6),
                                              which(unique(lee$id) == 22))]

# set number of presentations associated with categories 0 and 1 and save 
# participants number of trials
position_xy <- lee_filt |>
  dplyr::select(id, stimulus, category, category_char) |> 
  dplyr::distinct() |>
  dplyr::group_by(id) |> 
  dplyr::summarise("n_beta0" = sum(category == 0),
                   "n_beta1" = sum(category == 1))

# Assign labels to participants according to their number of trials
participant_labels <- LETTERS[seq(from = 1, to = length(unique(lee_filt$id)))]
participant_order <- lee_filt |> 
  dplyr::select(id, trial_condition) |> 
  dplyr::group_by(id) |> 
  dplyr::summarise("n_trials" = max(trial_condition)) |> 
  dplyr::pull(n_trials) |> 
  order(decreasing = FALSE)

# set up pdf
pdf(file = "figures/lee-navarro-type-4-filtered/posterior-stickiness.pdf", 
    width = 3, height = 3)

part_color<- c("#d72631", "#77c593", "#1868ae")
part_color_id <- c("A", "D", "H")

par(oma = c(2,2,0.2,0.2),
    mai = c(0,0,0,0))

mu_x <- c()
mu_y <- c()

plot(x = 0, y = 0, ann = FALSE, axes = FALSE, type = "n", 
     xlim = c(2,7.5), ylim = c(2, 7.5), asp = 1)
abline(a = 0, b = 1, lty = 3, col = "#939597")
box()
count <- 0

col_ci <- "#36454F"

for (i in 1:dim(beta_0)[2]) {
  if (position_xy$n_beta0[i] < 4) {
    
    qx <- quantile(x = beta_0[, i], probs = c(0.025, 0.975))
    
    qy <- quantile(x = beta_1[, i], probs = c(0.025, 0.975))
    
    mu_x <- append(x = mu_x, values = mean(x = beta_0[, i]))
    
    mu_y <- append(x = mu_y, values = mean(x = beta_1[, i]))
  }
  else {
    
    qx <- quantile(x = beta_1[, i], probs = c(0.025, 0.975))
    
    qy <- quantile(x = beta_0[, i], probs = c(0.025, 0.975))
    
    mu_x <- append(x = mu_x, values = mean(x = beta_1[, i]))
    
    mu_y <- append(x = mu_y, values = mean(x = beta_0[, i]))
  }
  
  arrows(x0 = mu_x[i], x1 = qx[1], 
         y0 = mu_y[i], y1 = mu_y[i], 
         angle = 90, length = 0.02, col = col_ci)
  
  arrows(x0 = mu_x[i], x1 = qx[2], 
         y0 = mu_y[i], y1 = mu_y[i], 
         angle = 90, length = 0.02, col = col_ci)
  
  arrows(x0 = mu_x[i], x1 = mu_x[i], 
         y0 = mu_y[i], y1 = qy[1],
         angle = 90, length = 0.02, col = col_ci)
  
  arrows(x0 = mu_x[i], x1 = mu_x[i], 
         y0 = mu_y[i], y1 = qy[2], 
         angle = 90, length = 0.02, col = col_ci)
}

points(x = mu_x[participant_order], y = mu_y[participant_order], pch = 21, 
       bg = c(part_color[1], rep(x = "#F5DF4D", 2),
              part_color[2], rep(x = "#F5DF4D", 3),
              part_color[3], rep(x = "#F5DF4D", 4)),
       col = "black",
       cex = 1.15)

text(x = mu_x[participant_order], y = mu_y[participant_order],
     labels = participant_labels, cex = 0.45)

axis(1, padj = -1.8, cex.axis = 0.8, tck = -0.02)
axis(2, las = 2, hadj = -0.5, cex.axis = 0.8, tck = -0.02)
mtext(text = "Low base-rate category", side = 1, line = 1, cex = 0.8)
mtext(text = "High base-rate category", side = 2, line = 1, cex = 0.8)

dev.off()
