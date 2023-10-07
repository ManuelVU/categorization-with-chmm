# Function that reads and writes data from one or more of the conditions in
# Bartlema, et al (2014) file is whiten in long-format as a .csv. There are 
# two designs with names Diagonal and Criss-cross which can be read and written
# into the directory ~/data/csv-files directory. 
bartlema_rw <- function (design = "Diagonal", 
                         add_transfer = TRUE, 
                         participants_keep = c(1:31),
                         file_suffix = "") {
  path <- paste(c("data/matlab-files/bartlema", design, ".mat"), collapse = "")
  
  x <- R.matlab::readMat(con = path)
  tmp <- x$d[,,1]
  category_names <- c("A", "B")
  
  n_participants <- tmp$nParticipants[1,1]
  t_condition <- c()
  
  for (pp in 1:n_participants) {
    index_p <- which(tmp$participant == pp)
    
    count_train <- 0
    
    for (tt in index_p) {
      
      if (tmp$trialType[tt, 1] == 1) {
        
        count_train <- count_train + 1
        
      } else if (tmp$trialType[tt, 1] == 2 & tmp$trialType[(tt - 1), 1] == 1) {
        
        count_train <- count_train + 1
        
      } else {
        
        count_train <- count_train
        
      }
      
      t_condition <- append(x = t_condition, values = count_train)
    }
  }
  
  design <- ifelse(test = design == "Diagonal", 
                   yes = "diagonal", 
                   no = ifelse(test = design == "CrissCross", 
                               yes = "crisscross", no = NULL))
  
  if (add_transfer == TRUE) {
    output <- dplyr::tibble(
      "id" = x$d[,,1]$participant[,1],
      "trial" = x$d[,,1]$trial[,1],
      "condition" = x$d[,,1]$trialType[,1],
      "condition_char" = unlist(x$d[,,1]$trialTypeLabels)[x$d[,,1]$trialType[,1]],
      "trial_condition" = t_condition, 
      "stimulus" = x$d[,,1]$stimulus[1,],
      "stimulus_char" = unlist(x$d[,,1]$stimulusLabels)[x$d[,,1]$stimulus[1,]],
      "response" = x$d[,,1]$response[,1] - 1,
      "response_char" = category_names[x$d[,,1]$response],
      "category" = x$d[,,1]$truth[,1] - 1,
      "category_char" = category_names[x$d[,,1]$truth],
      "correct" = x$d[,,1]$correct[,1])
    
    if (length(participants_keep) < 31) {
      output <- subset(x = output, subset = id %in% participants_keep)
    }
    
    readr::write_csv(x = output, 
                     file = paste(c("data/csv-files/bartlema-", 
                                    design, "-", file_suffix, ".csv"), 
                                  collapse = ""))
    
  } else if (add_transfer == FALSE) {
    output <- dplyr::tibble(
      "id" = x$d[,,1]$participant[,1],
      "trial" = x$d[,,1]$trial[,1],
      "condition" = x$d[,,1]$trialType[,1],
      "condition_char" = unlist(x$d[,,1]$trialTypeLabels)[x$d[,,1]$trialType[,1]],
      "trial_condition" = t_condition, 
      "stimulus" = x$d[,,1]$stimulus[1,],
      "stimulus_char" = unlist(x$d[,,1]$stimulusLabels)[x$d[,,1]$stimulus[1,]],
      "response" = x$d[,,1]$response[,1] - 1,
      "response_char" = category_names[x$d[,,1]$response],
      "category" = x$d[,,1]$truth[,1] - 1,
      "category_char" = category_names[x$d[,,1]$truth],
      "correct" = x$d[,,1]$correct[,1]) |>
      subset(subset = condition == 1)
    
    if (length(participants_keep) < 31) {
      output <- subset(x = output, subset = id %in% participants_keep)
    }
    
    readr::write_csv(x = output, 
                     file = paste(c("data/csv-files/bartlema-", 
                                    design, "-",file_suffix, "-notransfer.csv"), 
                                  collapse = ""))
    
  }
}
