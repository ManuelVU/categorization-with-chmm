plot_posterior_adequacy <- function (data, samples_y, order = "decreasing",
                                     category_color = c("#D81159", "#234E70")) {
  participants <- unique(data$id)
  trials <- c()
  count <- 0
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
       ylim = c(1,length(participants)), 
       xlim = c(1, max(trials[, 3])))
  
  for (p in 1:length(participants)) {
    agreement <- as.numeric(data$response[data$id == trials[p, 1]] == samples_y[trials[p, 2], 1:trials[p,3]])
    
    points(x = seq(1, trials[p, 3]), 
           y = rep(x = length(participants) + 1 - p, times = trials[p, 3]),
           pch = c(0,22)[(agreement + 1)], 
           col = category_color[(data$response[data$id == trials[p, 1]] + 1)],
           bg = category_color[(data$response[data$id == trials[p, 1]] + 1)])
  }
}
