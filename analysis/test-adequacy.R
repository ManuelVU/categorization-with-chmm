# Load data and posterior samples
lee_navarro <- transform_data_chmm(
  directory_data = "data/csv-files/lee-navarro-2002-type4.csv",
  directory_features = "data/stimulus-features/lee-navarro-features.csv")

samples <- readRDS(file = "analysis/posterior-samples/lee-navarro-2002-type4-posterior-samples.rds")

category_colors <- c("#EDC2D8FF", "#8ABAD3FF")

coherence_colors <- cbind(rep(x = "white", times = 2),
                          category_colors)

# Calculate modal state per trial
modal_state <- array(data = NA, 
                     dim = dim(samples$posterior_samples$hidden_states)[1:3])
for (i in 1:dim(modal_state)[3]) {
  modal_state[,,i] <- round(apply(
    X = samples$posterior_samples$hidden_states[,,i,],
    MARGIN = c(1,2), FUN = mean, na.rm =TRUE))
}

# Compare modal state with participants' responses
plot(x = 0, y = 0,axes = FALSE, ann = FALSE, type = "n", 
     ylim = c(1,dim(modal_state)[3]),
     xlim = c(1,dim(modal_state)[2]))
height = 0.8
width = 0.8
for (i in 1:dim(modal_state)[3]) {
  for (t in 1:lee_navarro$participant_t[i]) {
    row_index <- which(!is.na(lee_navarro$response[,t,i]))
    
    equality <- ifelse(test = (modal_state[row_index, t, i] ==
                               lee_navarro$response[row_index, t, i]),
                       yes = 1, no = 0)
    
    rect(xleft = t - width/2, ybottom = (20 - i) - height/2, 
         xright = t + width/2, ytop = (20 - i) + height/2, 
         border = category_colors[lee_navarro$response[row_index, t, i] + 1],
         col = coherence_colors[lee_navarro$response[row_index, t, i] + 1,
                                equality + 1])
  }
}

### Test some participants
participants <- c(1,2,3,4,5,6,8,9,12,13,17)


pdf(file = "figures/test-adequacy.pdf", width = 13, height = 8)
par(xaxs = "i")
plot(x = 0, y = 0,axes = FALSE, ann = FALSE, type = "n", 
     ylim = c(1,length(participants)), 
     xlim = c(0.3,lee_navarro$participant_t[1]))
axis(1)
axis(2, las = 2)
mtext(text = "Trial", side = 1, line = 2.8, cex = 1.7)
mtext(text = "Participant", side = 2, line = 2.5, cex = 1.7)
box(type = "l")
height <- 0.6
width <- 0.8
counter <- length(participants)

for (i in participants) {
  for (t in 1:lee_navarro$participant_t[i]) {
    row_index <- which(!is.na(lee_navarro$response[,t,i]))
    
    equality <- ifelse(test = (modal_state[row_index, t, i] ==
                                 lee_navarro$response[row_index, t, i]),
                       yes = 1, no = 0)
    
    rect(xleft = t - width/2, ybottom = counter - height/2, 
         xright = t + width/2, ytop = counter + height/2, 
         border = category_colors[lee_navarro$response[row_index, t, i] + 1],
         col = coherence_colors[lee_navarro$response[row_index, t, i] + 1,
                                equality + 1])
    
  }
  counter <- counter - 1
}
dev.off()

