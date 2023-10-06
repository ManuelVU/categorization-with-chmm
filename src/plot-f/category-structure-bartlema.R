upper.half.circle <- function(x,y,r,nsteps=100){  
  rs <- seq(0,pi,len=nsteps) 
  xc <- x+r*cos(rs) 
  yc <- y+r*sin(rs) 
  return(cbind(xc, yc))
} 


pdf(file = "figures/category-structure-bartlema.pdf", width = 4, height = 4)

par(mai = c(0,0,0,0),
    oma = c(2,2,2,2))
# layout(t(c(1,2)))

col_category <- c("#30626d", "#723f75")
col_transfer <- c("black", "#FCF6F5")

centers <- cbind(rep(seq(-0.75, 1, 0.5), times = 4),
                 rep(seq(-0.75, 1, 0.5), each = 4),
                 c(0, 1, 1, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0),
                 c(0, 1, 1, 0, 2, 0, 1, 0, 0, 2, 0, 1, 0, 2, 2, 0))
  
plot(x = 0, y = 0, type = "n", xlim = c(-1,1), ylim = c(-1, 1), asp = 1,
       axes = FALSE, ann = FALSE)
arrows(x0 = -1, y0 = -0.9, x1 = 1, y1 = -0.9, angle = 25, lwd = 1.5, 
       length = 0.1)
arrows(x0 = -1, y0 = -0.9, x1 = -1, y1 = 1, angle = 25, lwd = 1.5, 
       length = 0.1)
mtext(text = "Diagonal", cex = 1.5, line = -0.5)
mtext(text = "Size", cex = 1.2, line = -0.5, side = 2)
mtext(text = "Angle", cex = 1.2, line = -1, side = 1)

for(i in 1:dim(centers)[1]){
  half.cir <- upper.half.circle(x = centers[i, 1], y = centers[i, 2], r = 0.15)
  
  if(centers[i, 3] == 1) {
    rect(xleft = centers[i, 1] - 0.2, ybottom = centers[i, 2] - 0.05,
         xright = centers[i, 1] + 0.2, ytop = centers[i, 2] + 0.2, 
         col = col_category[centers[i, 4]], border = FALSE)
    lines(x = half.cir[,1], y = half.cir[,2], col = "#FCF6F5", lwd = 2)
    segments(x0 = centers[i, 1], x1 = half.cir[25,1], 
             y0 = centers[i,2], y1 = half.cir[25,2], col = "#FCF6F5", lwd = 2)
  }
  else {
    lines(x = half.cir[,1], y = half.cir[,2], lwd = 2)
    segments(x0 = centers[i, 1], x1 = half.cir[25,1], 
             y0 = centers[i,2], y1 = half.cir[25,2], lwd = 2)
  }
}

dev.off()
