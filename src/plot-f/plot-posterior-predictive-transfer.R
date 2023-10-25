# Ploting function for posterior predictive distribution for transfer phases

plot_pp_transfer <- function (data, path_posterior, path_comparison, 
                              transfer_id, participant_id,
                              representation, inner_width = 0.34, 
                              inner_height = 0.28, outer_lwd = 6, 
                              prediction_space =0.01, relative_pos = 0.06) {
  
  posterior <- readRDS(file = path_posterior)$y_hat
  
  posterior <- posterior[participant_id, ]
  
  comparison <- readRDS(file = path_comparison)
  
  col_category <- c("#30626d", "#723f75")
  
  # col_error <- c("#D81159bb", "#234E70bb")
  
  col_error <- c("white", "white")
  
  data <- subset(data, subset = id == participant_id)
  
  transfer_trials <- unique(data$trial_condition[which(data$condition_char == "transfer")])
  
  data <- subset(data, subset = trial_condition == transfer_trials[transfer_id])
  
  data <- data[order(data$stimulus),]
  
  centers <- cbind(rep(seq(-0.75, 1, 0.5), times = 4),
                   rep(seq(-0.75, 1, 0.5), each = 4),
                   data$response + 1)
  
  center_pred <- cbind(rep(relative_pos, times = dim(centers)[1]),
                       rep(-relative_pos, times = dim(centers)[1]))
  
  plot(x = 0, y = 0, type = "n", xlim = c(-1,1), ylim = c(-1, 1), asp = 1,
       axes = FALSE, ann = FALSE)
  arrows(x0 = -1, y0 = -1, x1 = 1, y1 = -1, angle = 25, lwd = 1.5, 
         length = 0.1)
  arrows(x0 = -1, y0 = -1, x1 = -1, y1 = 1, angle = 25, lwd = 1.5, 
         length = 0.1)
  
  for(i in 1:dim(centers)[1]){
    rect(xleft = centers[i, 1] - 0.2, ybottom = centers[i, 2] - 0.18,
         xright = centers[i, 1] + 0.2, ytop = centers[i, 2] + 0.18, 
         border = col_category[centers[i, 3]], lwd = outer_lwd)
    
    rect(xleft = centers[i, 1] - inner_width / 2, 
         xright = centers[i, 1] - inner_width / 2 + posterior[i] * inner_width,
         ytop = centers[i, 2] + center_pred[i, 1] + inner_height / 2, 
         ybottom = centers[i, 2] + center_pred[i, 1] - inner_height / 2,
         border = FALSE, 
         col = ifelse(test = centers[i, 3] == 2, 
                      yes = col_category[2], 
                      no = col_error[2]))
    
    rect(xleft = centers[i, 1] - inner_width / 2 + posterior[i] * inner_width, 
         xright = centers[i, 1] + inner_width / 2,
         ytop = centers[i, 2] + center_pred[i, 1] + inner_height / 2, 
         ybottom = centers[i, 2] + center_pred[i, 1] - inner_height / 2,
         border = FALSE, col = ifelse(test = centers[i, 3] == 1, 
                                      yes = col_category[1], 
                                      no = col_error[1]))
    
    rect(xleft = centers[i, 1] - inner_width / 2, 
         xright = centers[i, 1] - inner_width / 2 + comparison[participant_id, i] * inner_width,
         ytop = centers[i, 2] + center_pred[i, 2] + inner_height / 2, 
         ybottom = centers[i, 2] + center_pred[i, 2] - inner_height / 2,
         border = FALSE, col = ifelse(test = centers[i, 3] == 2, 
                                      yes = col_category[2], 
                                      no = col_error[2]))
    
    rect(xleft = centers[i, 1] - inner_width / 2 + comparison[participant_id, i] * inner_width, 
         xright = centers[i, 1] + inner_width / 2,
         ytop = centers[i, 2] + center_pred[i, 2] + inner_height / 2, 
         ybottom = centers[i, 2] + center_pred[i, 2] - inner_height / 2,
         border = FALSE, col = ifelse(test = centers[i, 3] == 1, 
                                      yes = col_category[1], 
                                      no = col_error[1]))
    
    # lines(x = half.cir[,1], y = half.cir[,2], col = "#FCF6F5", lwd = 2)
    # segments(x0 = centers[i, 1], x1 = half.cir[25,1], 
    #          y0 = centers[i,2], y1 = half.cir[25,2], col = "#FCF6F5", lwd = 2)
  }
}
