# Look for participants who learn the task in less than 100 trials

lee <- readr::read_csv(file = "data/csv-files/lee-navarro-2002-type4.csv")

total_trials <- matrix(data = NA, nrow = length(unique(lee$id)), ncol = 2)

count <- 0

for (i in unique(lee$id)) {
  count <- count + 1
  
  total_trials[count, ] <- c(i,
                             max(subset(x = lee, subset = id == i)$trial_condition))
  
}

participants_to_keep_lee <- total_trials[which(total_trials[,2] < 100), 1]
