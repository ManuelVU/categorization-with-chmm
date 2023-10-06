pdf(file = "figures/posterior-adequacy-lee-navarro-type4.pdf", width = 8, 
    height = 4)

par(mai = c(0.1,0.1, 0, 0),
    oma = c(2, 2, 0.1,0.1))

plot_posterior_adequacy(data = lee_navarro, samples_y = mode_y, order = "null")

dev.off()
