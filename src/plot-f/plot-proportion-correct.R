# Ploting function moving average correct responses
correct_prop_trial <- function (data, participant, window_size, 
                                section) {
  if (missing(section)) {
    data <- subset(x = data, subset = data$id == participant)
  }
  else {
    data <- subset(x = data, subset = data$id == participant &
                                      data$trial_condition >= section[1] &
                                      data$trial_condition < section[2])
  }
  
  trials <- length(data$trial)
  
  y <- c()
  
  count <- 0
  
  for (t in window_size:trials) {
    count <- count + 1
    
    y[count] <- mean(data$correct[(t - window_size):t])
  }
  
  return(cbind(seq(window_size, trials), data$trial[window_size:trials], y))
}

plot_correct_prop <- function (data_plot, window_size, line_color, line_lwd,
                               point_color, add_hist = TRUE, hist_color,
                               sections, n_breaks = 5) {
  
  max_trials <- max(data_plot$trial_condition)
  if (add_hist == TRUE) {
    plot(0,0,type = "n", ylim = c(0,1.4), 
         xlim = c(1,max_trials + 10), axes = FALSE, 
         ann = FALSE)
  }
  else {
    plot(0,0,type = "n", ylim = c(0,1), 
         xlim = c(1,max_trials), axes = FALSE, 
         ann = FALSE)
  }
  
  tt <- c()
  pp <- c()

  count <- 0
  
  for (n in unique(data_plot$id)) {
    count <- count + 1
    
    if (!missing(sections)) {
      for (j in 1:nrow(sections)) {
        plot_data <- correct_prop_trial(data = data_plot, participant = n, 
                                        window_size = window_size, 
                                        section = sections[j, ])
        
        lines(x = plot_data[, 2], y = plot_data[, 3], col = line_color, 
              lwd = line_lwd)
        
      }
    }
    else {
      plot_data <- correct_prop_trial(data = data_plot, participant = n, 
                                      window_size = window_size)
      
      tt[count] <- max(plot_data[, 1])
      pp[count] <- plot_data[length(plot_data[, 1]), 3]
      
      lines(x = plot_data[, 1], y = plot_data[, 3], col = line_color, 
            lwd = line_lwd)
    }
  }
  
  if (add_hist == TRUE) {
    points(x = tt, y = pp,
           bg = point_color, col = "#FCF6F5", pch = 23, cex = 0.8)
    
    hx <- hist(tt, breaks = n_breaks, plot = FALSE)
    
    for (k in 2:length(hx$breaks)) {
      if (hx$counts[(k - 1)] > 0) {
        rect(xleft = hx$breaks[(k - 1)], xright = hx$breaks[k], ybottom = 1.02, 
             ytop = 1.02 + hx$density[(k - 1)]/max(hx$density) * 0.37, 
             border = "#FCF6F5", col = hist_color)
        
      }
    }  
  }
}
