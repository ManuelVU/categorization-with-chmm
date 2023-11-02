# Figure of Lee and Navarro's type 4 category structure

pdf(file = "figures/lee-navarro-type-4-filtered/category-structure.pdf", 
    width = 3.8, height = 3.8)

par(oma = c(0.1,0.1,0.1,0.1),
    mai = c(0.1,0.1,0.1,0.1),
    yaxs = "i", 
    xaxs = "i")

st_col <- c("#d72631", "#77c593", "#1868ae")

plot(x = 0, y = 0, ann = FALSE, axes = FALSE, type = "n", asp = 1, 
     xlim = c(-0.3,7.3), ylim = c(-0.3,7.3))

# small category
points(x = 3.5, y = 6.5, pch = 16, col = "#2e3036", cex = 8)
points(x = 0.5, y = 0.5, pch = 15, col = "#2e3036", cex = 8)
points(x = 6.5, y = 0.36, pch = 17, col = "#2e3036", cex = 7.5)

# circles
points(x = 3.5, y = 6.5, pch = 16, col = st_col[1], cex = 6)
points(x = 2.5, y = 4.5, pch = 16, col = st_col[2], cex = 6)
points(x = 4.5, y = 4.5, pch = 16, col = st_col[3], cex = 6)

# squares
points(x = 1.5, y = 2.5, pch = 15, col = st_col[1], cex = 6)
points(x = 0.5, y = 0.5, pch = 15, col = st_col[2], cex = 6)
points(x = 2.5, y = 0.5, pch = 15, col = st_col[3], cex = 6)

# triangles
points(x = 5.5, y = 2.36, pch = 17, col = st_col[1], cex = 4.8)
points(x = 4.5, y = 0.36, pch = 17, col = st_col[2], cex = 5)
points(x = 6.5, y = 0.36, pch = 17, col = st_col[3], cex = 5)


# abline(h = seq(1,6), lty = 2)
# abline(v = seq(1,6), lty = 2)
# axis(1)
# axis(2)
# box()

dev.off()