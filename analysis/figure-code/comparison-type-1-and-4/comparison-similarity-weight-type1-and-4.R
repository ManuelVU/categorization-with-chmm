# We want to compare the posterior distribution of the similarity weight 
# parameter across participants in the two conditions. 

# First, I need to find the id's of participants in each condition.

ids_remove <- c(2, 6)

participant_labels <- LETTERS[seq(from = 1, to = 12)]

type1 <- readr::read_csv(
  file = "data/csv-files/lee-navarro-2002-type1-filtered.csv") |>
  dplyr::group_by(id) |> 
  dplyr::select("id", "correct") |>
  dplyr::summarise("total_correct" = sum(correct), 
                   "n" = length(correct)) |>
  dplyr::filter(!id %in% ids_remove) |>
  dplyr::mutate("position" = seq(from = 1, to = 12),
                "probability" = total_correct / n)

type4 <- readr::read_csv(
  file = "data/csv-files/lee-navarro-2002-type4-filtered.csv") |>
  dplyr::group_by(id) |> 
  dplyr::select("id", "correct") |>
  dplyr::summarise("total_correct" = sum(correct), 
                   "n" = length(correct)) |>
  dplyr::filter(!id %in% ids_remove) |>
  dplyr::mutate("position" = seq(from = 1, to = 12),
                "probability" = total_correct / n)

theta <- cbind(type1$position, log(type1$probability/type4$probability))

theta <- theta[order(theta[, 2], decreasing = TRUE), ]

ids_remove <- c("V1", "V4")

samples_type1 <- readRDS(
  file = paste(c("data/posterior-samples/model-parameters/",
                 "lee-navarro-type1-filtered-chmm-stick-weight.rds"),
               collapse = ""))$posterior_samples$kappa |>
  as.data.frame() |>
  dplyr::select(-all_of(ids_remove))

kappa_type1 <- readRDS(
  file = paste(c("data/posterior-samples/model-parameters/",
                 "lee-navarro-type1-filtered-chmm-stick-weight.rds"),
               collapse = ""))$posterior_samples$kappa |>
  as.data.frame() |>
  dplyr::select(-all_of(ids_remove)) |>
  apply(MARGIN = 2, FUN = quantile, p = c(0.025, 0.5, 0.975))

samples_type4 <- readRDS(
  file = paste(c("data/posterior-samples/model-parameters/",
                 "lee-navarro-type4-filtered-chmm-stick-weight.rds"),
               collapse = ""))$posterior_samples$kappa |>
  as.data.frame() |>
  dplyr::select(-all_of(ids_remove))

kappa_type4 <- readRDS(
  file = paste(c("data/posterior-samples/model-parameters/",
                 "lee-navarro-type4-filtered-chmm-stick-weight.rds"),
               collapse = ""))$posterior_samples$kappa |>
  as.data.frame() |>
  dplyr::select(-all_of(ids_remove)) |>
  apply(MARGIN = 2, FUN = quantile, p = c(0.025, 0.5, 0.975))

pdf(file = "figures/posterior-samples/comparison-quantiles.pdf",
    width = 4.5, height = 3.4)
par(las = 1,
    oma = c(2.5,0,0.5,0.5),
    mai = c(0,0.45,0,0))

plot(x = 0, y = 0, ann = FALSE, axes = FALSE, type = "n", xlim = c(0.8, 12.2),
     ylim = c(min(kappa_type1, kappa_type4), max(kappa_type1, kappa_type4)))

category_col <- c("#30626d", "#723f75")

for (i in 1:dim(kappa_type1)[2]) {
  k <- type4$position[i]
  segments(x0 = i - 0.1, x1 = i - 0.1, 
           y0 = kappa_type1[1, k], y1 = kappa_type1[3, k],
           col = category_col[1], lwd = 2.5)
  
  points(x = i - 0.1, y = kappa_type1[2, k], cex = 1.1, pch = 21,
         bg = category_col[1], col = "white")
  
  segments(x0 = i + 0.1, x1 = i + 0.1, 
           y0 = kappa_type4[1, k], y1 = kappa_type4[3, k],
           col = category_col[2], lwd = 2.5)
  
  points(x = i + 0.1, y = kappa_type4[2, k], cex = 1.1, pch = 21,
         bg = category_col[2], col = "white")
}
box(bty = "l")
axis(side = 1, at = seq(from = 1, to = 12), labels = participant_labels,
     padj = -1.5, cex.axis = 0.9, tck = -0.017)
axis(side = 2, tck = -0.02, cex.axis = 0.9, hadj = -0.3)
legend("topright", legend = c("type I", "type IV"), col = category_col,
       pch = 19, bty = "n", cex = 1.1)
mtext(text = "Similarity weight", side = 2, line = 1.15, cex = 1.4, las = 0)
mtext(text = "Participant", side = 1, line = 1.5, cex = 1.4)
dev.off()

