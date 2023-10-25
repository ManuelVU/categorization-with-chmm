lee <- readr::read_csv(
  file = "data/csv-files/lee-navarro-2002-type4-filtered.csv")

samples <- readRDS(file = paste(c("data/posterior-samples/posterior-adequacy/",
                                  "lee-navarro-type4-filtered-chmm.rds"),
                                collapse = ""))

pdf(file = "figures/lee-navarro-type-4-filtered/posterior-adequacy-lee-navarro.pdf",
    width = 8.3, height = 3.8)

par(oma = c(2,2.1,0.1,0.1),
    mai = c(0.1,0.1,0.1,0.1),
    xaxs = "i", yaxs = "i")

source(file = "src/plot-f/plot-posterior-adequacy.R")

plot_posterior_adequacy(data = lee, samples_y = samples, order = "decreasing", 
                        category_color = c("#30626d", "#723f75"), width = 0.67,
                        height = 0.6)
box(bty = "l")
axis(1, at = c(1, seq(1,100, 10)), padj = -1.3, tck = -0.02, cex = 1)

mtext(text = "Trial", side = 1, outer = TRUE, cex = 1.3, line = 1)
mtext(text = "Participant", side = 2, outer = TRUE, cex = 1.3, line = 0)
dev.off()
