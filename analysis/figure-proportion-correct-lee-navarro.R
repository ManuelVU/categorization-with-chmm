# Read Lee and Navarro's data for type 4 category structure
lee_navarro <- readr::read_csv(file = "data/csv-files/lee-navarro-2002-type4.csv")

# Load ploting function
source(file = "src/plot-f/plot-proportion-correct.R")

# Figure as pdf
pdf(file = "figures/proportion-correct-lee-navarro.pdf", width = 5, height = 4)

par(oma = c(2.5,2.5,0.1,1), 
    mai = c(0.1,0.2,0,0),
    yaxs = "i", 
    xaxs = "i")

plot_correct_prop(data_plot = lee_navarro, window_size = 10, 
                  line_color = "#234E70aa", line_lwd = 1.3, 
                  point_color = "#234E70", hist_color = "#234E70")

axis(1, at = c(1, seq(50,300, 50)), padj = -1, labels = c("", seq(50,300, 50)))
axis(2, at = seq(0,1, 0.2), labels = c("0", seq(0.2, 0.8, 0.2), "1"),
     las = 2, hadj = 0.7)

mtext(text = "Proportion of correct", side = 2, cex = 1.3, line = 2.3, 
      at = 0.51)

mtext(text = "Trials", side = 1, line = 1.8, cex = 1.3)

dev.off()
