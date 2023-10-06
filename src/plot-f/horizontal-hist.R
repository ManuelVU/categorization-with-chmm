# Function that takes two vectors and generates two side by side horizontal 
# histograms

horizontal_hist <- function (data_x1, data_x2, height = seq(0, 14, 0.5), 
                             center, width = 0.45, 
                             col_hist = c("#30626d", "#723f75")) {
  hx1 <- hist(x = data_x1, plot = FALSE, breaks = height)
  hx2 <- hist(x = data_x2, plot = FALSE, breaks = height)
  
  rect(xleft = rep(x = center, times = length(hx1$density)) - 
         hx1$density/max(hx1$density) * width, 
       xright = rep(x = center, times = length(hx1$density)),
       ybottom = hx1$breaks[-length(hx1$breaks)],
       ytop = hx1$breaks[-1],
       border = FALSE, col = col_hist[1])
  
  rect(xleft = rep(x = center, times = length(hx2$density)), 
       xright = rep(x = center, times = length(hx2$density)) + 
         hx2$density/max(hx2$density) * width,
       ybottom = hx2$breaks[-length(hx2$breaks)],
       ytop = hx2$breaks[-1],
       border = FALSE, col = col_hist[2])
}
