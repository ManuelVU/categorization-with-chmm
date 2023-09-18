# Ploting function moving average correct responses
correct_prop_trial <- function (data, participant, window_size) {
  data <- subset(x = data, subset = data$id == participant)
  trials <- max(data$trial_condition)
  y <- c()
  count <- 0
  for (t in window_size:trials) {
    count <- count + 1
    y[count] <- mean(data$correct[(t - window_size):t])
  }
  return(cbind(seq(window_size, trials),y))
}

plot_correct_prop <- function (data_plot, window_size, line_color, line_lwd,
                               point_color, add_hist = TRUE, hist_color) {
  
  max_trials <- max(lee_navarro$trial_condition,300)
  
  plot(0,0,type = "n", ylim = c(0,1.4), xlim = c(1,max_trials), axes = FALSE, 
       ann = FALSE)
  
  tt <- c()
  pp <- c()

  count <- 0
  
  for (n in unique(lee_navarro$id)) {
    count <- count + 1
    
    plot_data <- correct_prop_trial(data = lee_navarro, participant = n, 
                                    window_size = 9)
    
    tt[count] <- max(plot_data[,1])
    pp[count] <- plot_data[length(plot_data[,1]),2]
    
    lines(x = plot_data[, 1], y = plot_data[, 2], col = line_color, 
          lwd = line_lwd)
  }
  
  points(x = tt, y = pp,
         bg = point_color, col = "#FCF6F5", pch = 23, cex = 0.8)
  
  hx <- hist(tt, breaks = seq(0,300,20), plot = FALSE)
  
  for (k in 2:length(hx$breaks)) {
    if (hx$counts[(k - 1)] > 0) {
      rect(xleft = hx$breaks[(k - 1)], xright = hx$breaks[k], ybottom = 1.02, 
           ytop = 1.02 + hx$density[(k - 1)]/max(hx$density) * 0.37, 
           border = "#FCF6F5", col = hist_color)
      
    }
  }
  
}
