plot_posterior_adequacy <- function (data, samples_y, order = "decreasing",
                                     category_color = c("#D81159", "#234E70"),
                                     fill_color = NA,
                                     width = 1, height = 1, increase = 3) {
  participants <- unique(data$id)
  trials <- c()
  count <- 0
  
  filling <- rbind(c(fill_color, fill_color), 
                   category_color)
  
  for (i in participants) {
    count <- count + 1
    trials[count] <- max(data$trial_condition[data$id == i])
  }
  
  trials <- cbind(participants, seq(1, length(participants)), trials)
  
  if (order == "decreasing") {
    trials <- trials[order(trials[, 3], decreasing = TRUE), ]
  } 
  else if (order == "increasing") {
    trials <- trials[order(trials[, 3]), ]
  } 
  else {
    trials <- trials
  }
  
  plot(x = 0, y = 0, ann = FALSE, axes = FALSE, type = 'n', 
       ylim = c(0.5, length(participants) + 0.5), 
       xlim = c(0, max(trials[, 3]) + increase))
  
  for (p in 1:length(participants)) {
    tmp <- subset(x = data, subset = id == trials[p, 1])
    
    for (t in 1:trials[p, 3]) {
      boder_color <- category_color[tmp$response[t] + 1]
      
      agreement <- round(samples_y[trials[p, 2], t]) == tmp$response[t]
      
      filling_color <- filling[agreement + 1, tmp$response[t] + 1]
      
      rect(xleft = t - width / 2, 
           xright = t + width / 2, 
           ybottom = length(participants) + 1 - p - height / 2,
           ytop = length(participants) + 1 - p + height / 2,
           border = boder_color,
           col = filling_color)      
    }
  }
}
