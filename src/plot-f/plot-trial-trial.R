# Plotting function for trial by trial responses single participant

trial_trial_participant <- function (data, posteriors, participant_id,
                                     posterior_add = FALSE, transfer = FALSE,
                                     stimulus_plot, plot_order,
                                     category_color, border_color,
                                     width = 0.8, 
                                     height = 0.8, 
                                     shade_stimulus = FALSE, 
                                     region = c(0.05,0.05,0.05,1), 
                                     color_shade = "#9FB1BC55", 
                                     posterior_margin = 0.08, lwd_rect = 0.1,
                                     bar_width = 0.1, 
                                     transparency_bars = TRUE, 
                                     x_lim) {
  
  n_trials <- data$participant_t[participant_id]
  
  if(missing(x_lim)){
    x_lim = c(0, n_trials + 1)
  }
  
  rect_color <- category_color
  
  if (missing(plot_order)){
    plot_order <- 1:dim(data$response)[1]
  }
  
  if (missing(stimulus_plot)) {
    responses <- data$response[, 1:n_trials, participant_id]
    responses <- responses[plot_order, ]
  }
  else {
    responses <- data$response[stimulus_plot, 1:n_trials, participant_id]
  }
  
  n_stimulus <- dim(responses)[1]
  
  if (posterior_add == TRUE) {
    if (!missing(posteriors)) {
      if (missing(stimulus_plot)) {
        states_tmp <- 
          posteriors$posterior_samples$hidden_states[, ,participant_id, ]
        
        states_tmp <- states_tmp[, 1:n_trials, ]
        
        states_tmp <- states_tmp[plot_order, , ]
      }
      else {
        states_tmp <- 
          posteriors$posterior_samples$hidden_states[, ,participant_id, ]
        
        states_tmp <- states_tmp[stimulus_plot, 1:n_trials, ]
        
        states_tmp <- states_tmp[plot_order, , ]
      }
    }
  }
  
  plot(x = 0, y = 0, type = "n", ann = FALSE, axes = FALSE, 
       xlim = x_lim, ylim = c(0,n_stimulus + height))
  
  if (shade_stimulus == TRUE) {
    for (i in 1:n_stimulus) {
        rect(xleft = - (width / 2 + region[2]), 
             ybottom = i - 1 + height / 2 + region[1],
             xright = n_trials + width / 2 + region[4],
             ytop = i - height / 2 - region[3], border = FALSE, 
             col = color_shade)
    }
  }
  
  if (transfer == FALSE) {
    for(tt in 1:n_trials) {
      rect(xleft = tt - width / 2, 
           ybottom = which(!is.na(responses[, tt])) - height / 2,
           xright = tt + width / 2, 
           ytop = which(!is.na(responses[, tt])) + height / 2,
           border = border_color,
           col = 
             category_color[responses[which(!is.na(responses[, tt])), tt] + 1],
           lwd = lwd_rect)
      
    }  
  }
  
  if (posterior_add == TRUE) {
    states_mean <- apply(X = states_tmp, MARGIN = c(1, 2), FUN = mean)
    a <- 0:(n_stimulus - 1) + height / 2 + posterior_margin
    b <- 1:n_stimulus - height / 2 - posterior_margin
    mid <- a + (b - a) / 2
    
    
    for(tt in 1:n_trials) {
      rect(xleft = rep(x = tt, times = n_stimulus) - bar_width / 2,
           xright = rep(x = tt, times = n_stimulus) + bar_width / 2,
           ybottom = ifelse(test = states_mean[, tt] > 0.5, 
                            yes = mid, 
                            no = a + states_mean[, tt] * (b - a)),
          ytop = ifelse(test = states_mean[, tt] > 0.5, 
                        yes = a + states_mean[, tt] * (b - a), 
                        no = mid),
          col = rect_color[round(states_mean[, tt]) + 1],
          border = "black", lwd = 1.3)
      
    }
    # abline(h = mid, col = "#FCF6F5FF", lwd = 1.83)
    segments(x0 = rep(x = 0, times = length(mid)),
             x1 = rep(x = n_trials + 1, times = length(mid)),
             y0 = mid,
             y1 = mid,
             col = "#FCF6F5FF", lwd = 1.83)
    
  }
}

