# Plot proportion of correct for bartlema diagonal
source(file = "src/plot-f/plot-proportion-correct.R")

bart <- readr::read_csv(
  file = "data/csv-files/bartlema-crisscross-filtered.csv")

bart <- subset(x = bart, subset = condition_char != "transfer")

pdf(file = paste(c("figures/bartlema-crisscross-filtered/",
                   "proportion-correct-plot-bartlema-crisscross.pdf"), 
                 collapse = ""),
    width = 8.3, height = 3.8)

par(oma = c(2.5,2.5,0.1,1),
    mai = c(0.1,0.2,0,0),
    yaxs = "i", 
    xaxs = "i")

plot_correct_prop(data_plot = bart, window_size = 16,
                  line_color = "#30626d99", line_lwd = 1.3, 
                  point_color = "#30626d", add_hist = FALSE)

axis(side = 1, at = c(1, seq(20,400, 20)), padj = -1, 
     labels = c("", seq(20,400, 20)))
axis(side = 2, at = seq(0,1, 0.2), labels = c("0", seq(0.2, 0.8, 0.2), "1"),
     las = 2, hadj = 0.7)

mtext(text = "Proportion of correct", side = 2, cex = 1.3,
      line = 2.3, at = 0.51)
mtext(text = "Trials", side = 1, line = 1.8, cex = 1.3)

dev.off()
