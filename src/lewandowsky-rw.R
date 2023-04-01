# Function that reads and writes data from one or more conditions in
# Lewandowsky (2011), file is whiten in long-format as a .csv into the 
# ~/data/csv-files directory. (requires the file: 
# data/csv-files/lewandowsky-categories.csv)
lewandowsky_rw <- function(file_suffix, 
                           conditions_keep = c(1:6), 
                           participants_keep = c(1:113)){
  cats <- readr::read_csv(file = "data/csv-files/lewandowsky-categories.csv")
  
  tmp <- R.matlab::readMat(con = "data/matlab-files/Lewandowsky2011Data.mat")
  tmp <- tmp$d[,,1]
  
  cats <- c("x", "y")
  
  indx_stim <- apply(X = cbind(
    rep(x = c("gray-", "white-"), each = 4), 
    rep(x = c("triangle-", "square-"), times = 4),
    rep(x = c("large", "small", "large","small"), each = 2)),
    MARGIN = 1, FUN = paste, collapse = "")
  
  id <- c()
  trial <- c()
  condition <- c()
  trial_condition <- c()
  stimulus <- c()
  stimulus_char <- c()
  response <- c()
  response_char <- c()
  category <- c()
  category_char <- c()
  correct <- c()
  
  for(i in participants_keep){
    n_correct <- rbind(colSums(tmp$correct1[i,,]), colSums(tmp$correct2[i,,]),
                       colSums(tmp$correct3[i,,]), colSums(tmp$correct4[i,,]),
                       colSums(tmp$correct5[i,,]), colSums(tmp$correct6[i,,]))
    tt <- c()

    for(j in 1:6){
      tt[j] <- c(min(which((dplyr::lag(n_correct[j,] == 16, n = 1) & 
                                       n_correct[j,] == 16) == TRUE)))
      
      tt[j] <- ifelse(test = is.infinite(tt[j]), yes = 12, no = tt[j])
      
      st <- as.vector(t(tmp$stim[i,1:tt[j],,j]))
      
      stimulus <- append(x = stimulus, values = st)
      
      stimulus_char <- append(x = stimulus_char, values = indx_stim[st])
      
      category <- append(x = category, values = tmp$cs[j,st] - 1)
      
      category_char <- append(x = category_char, values = cats[tmp$cs[j,st]])
    }
    
    id <- append(x = id, values = rep(x = i, times = sum(tt * 16)))
    
    trial <- append(x = trial, values = seq(1, sum(tt * 16)))
    
    condition <- append(x = condition, values = rep(x = 1:6, times = tt * 16))
    
    trial_condition <- append(x = trial_condition, 
                              values = 
                                unlist(sapply(X = tt * 16, FUN = seq, 
                                              from = 1)))
    
    crt <- c(as.vector(as.logical(tmp$correct1[i,,1:tt[1]])),
             as.vector(as.logical(tmp$correct2[i,,1:tt[2]])),
             as.vector(as.logical(tmp$correct3[i,,1:tt[3]])),
             as.vector(as.logical(tmp$correct4[i,,1:tt[4]])),
             as.vector(as.logical(tmp$correct5[i,,1:tt[5]])),
             as.vector(as.logical(tmp$correct6[i,,1:tt[6]])))
    
    st <- c(tmp$cs[1, as.vector(t(tmp$stim[i,1:tt[1],,1]))],
            tmp$cs[2, as.vector(t(tmp$stim[i,1:tt[2],,2]))],
            tmp$cs[3, as.vector(t(tmp$stim[i,1:tt[3],,3]))],
            tmp$cs[4, as.vector(t(tmp$stim[i,1:tt[4],,4]))],
            tmp$cs[5, as.vector(t(tmp$stim[i,1:tt[5],,5]))],
            tmp$cs[6, as.vector(t(tmp$stim[i,1:tt[6],,6]))])
    
    correct <- append(x = correct, values = crt)
    
    response <- append(x = response,
                       values = ifelse(test = crt == TRUE,
                                       yes = st - 1, 
                                       no = 1 - (st - 1)))
    
    response_char <- append(x = response_char, 
                            values = ifelse(test = crt == TRUE, 
                                            yes = cats[st], 
                                            no = cats[((1 - (st - 1)) + 1)]))
    
  }
  
  out_tmp <- dplyr::tibble(id, trial, condition, trial_condition,
                           stimulus, stimulus_char, response, response_char, 
                           category, category_char, correct)
  
  if(length(conditions_keep) < 6){
    out_tmp <- subset(x = out_tmp, 
                      subset = out_tmp$condition %in% conditions_keep)
  }
  
  if(missing(file_suffix)){
    file_suffix <- "all"
  }
  
  readr::write_csv(x = out_tmp, 
                   file = paste(c("data/csv-files/lewandowsky-2011-", 
                                  file_suffix, ".csv"), collapse = ""))
}
