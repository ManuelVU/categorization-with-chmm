# Plotting function for transfer trial responses single participant

plot_transfer <- function (data, posteriors, participant, transfer_number,
                           category_index,
                           color_category, color_transfer, color_border, 
                           cex_point, width_st = 0.2, height_st = 0.2, 
                           sep = 0.02) {
  
  
  features <- as.matrix(data$stimulus_features[, c(3, 4)])
  
  transfer_index <- which(x = 
                            apply(X = !is.na(data$response[, , participant]),
                                  MARGIN = 2, FUN = sum, na.rm = TRUE) > 1)
  
  states_tmp <- 
    posteriors$posterior_samples$hidden_states[, transfer_index[transfer_number],
                                               participant, ] |> 
    rowMeans()
  
  response <- data$response[, transfer_index[transfer_number], participant]
  
  plot(x = 0, y = 0, axes = FALSE, ann = FALSE, type = "n", 
       xlim = c(min(features[, 1]), 
                max(features[, 1])),
       ylim = c(min(features[, 2]) - height_st / 1.5, 
                max(features[, 2]) + 0.72), 
       asp = 1)
  
  rect(xleft = features[category_index, 1] - width_st / 2, 
       ybottom = features[category_index, 2] - height_st / 2,
       xright = features[category_index, 1] + width_st / 2,
       ytop = features[category_index, 2] + height_st / 2, 
       col = color_category[response[category_index] + 1], 
       border = color_border)
  
  segments(x0 = features[, 1] - width_st / 4,
           y0 = features[, 2] + height_st / 2 + sep,
           x1 = features[, 1] - width_st / 4,
           y1 = features[, 2] + height_st / 2 + sep + (1 - states_tmp) * 0.5,
           col = color_category[1], lwd = 2)
  
  segments(x0 = features[, 1] + width_st / 4,
           y0 = features[, 2] + height_st / 2 + sep,
           x1 = features[, 1] + width_st / 4,
           y1 = features[, 2] + height_st / 2 + sep + states_tmp * 0.5,
           col = color_category[2], lwd = 2)
  
  # rect(xleft = features[-category_index, 1] - width_st / 2, 
  #      ybottom = features[-category_index, 2] - height_st / 2,
  #      xright = features[-category_index, 1] + width_st / 2,
  #      ytop = features[-category_index, 2] + height_st / 2, 
  #      col = color_transfer,
  #      border = color_border)
  
  circles_xy <- cbind(features[-category_index, 1], 
                      features[-category_index, 2])
  tmp_index <- seq(1,16)[-category_index]
  
  for (i in 1:dim(circles_xy)[1]) {
    plotrix::draw.circle(x = circles_xy[i, 1],
                         y = circles_xy[i, 2],
                         radius = width_st / 2, 
                         border = color_category[response[tmp_index[i]] + 1],
                         col = color_transfer)    
  }
  

  
  # points(x = features[category_index, 1], y = features[category_index, 2], 
  #        pch = 21, col = color_border, 
  #        bg = color_category[response[category_index] + 1], 
  #        cex = cex_point)
  # 
  # points(x = features[-category_index, 1], 
  #        y = features[-category_index, 2], 
  #        pch = 21, col = color_border, 
  #        bg = color_transfer, 
  #        cex = cex_point)
}
