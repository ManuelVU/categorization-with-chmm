# This function reads and writes data from one or more of the conditions
# In Lee and Navarro (2002) and saves them in long-format. 
lee_navarro_rw <- function(file_suffix,
                           conditions_keep = c(1:4), 
                           participants_keep = c(1:22)){
  
  tmp <- R.matlab::readMat(con = "data/matlab-files/LeeNavarro2002Data.mat")
  tmp <- tmp$d[,,1]
  indx_stim <- apply(X = cbind(
    rep(x = c("circle-", "square-", "triangle-"), each = 3),
    rep(x = c("red", "green", "blue"), times = 3)), 
    MARGIN = 1, FUN = paste, collapse = "")
  
  cats <- c("x", "y")
  
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
    tt <- c(max(which(!is.na(tmp$y1[i, ]))),
            max(which(!is.na(tmp$y2[i, ]))),
            max(which(!is.na(tmp$y3[i, ]))),
            max(which(!is.na(tmp$y4[i, ]))))
    
    st <- c(tmp$stimindex1[i, 1:tt[1]], tmp$stimindex2[i, 1:tt[2]], 
            tmp$stimindex3[i, 1:tt[3]], tmp$stimindex4[i, 1:tt[4]])
    
    rs <- c(tmp$y1[i, 1:tt[1]], tmp$y2[i, 1:tt[2]], 
            tmp$y3[i, 1:tt[3]], tmp$y4[i, 1:tt[4]])
    
    ct <- c(tmp$cs[tmp$stimindex1[i, 1:tt[1]],1], 
            tmp$cs[tmp$stimindex2[i, 1:tt[2]],2],
            tmp$cs[tmp$stimindex3[i, 1:tt[3]],3],
            tmp$cs[tmp$stimindex4[i, 1:tt[4]],4])
    
    id <-  append(x = id, values = rep(x = i, times = sum(tt)))
    
    trial <- append(x = trial, values = seq(1, sum(tt)))
    
    condition <- append(x = condition, values = rep(x = 1:4, times = tt))
    
    trial_condition <- append(x = trial_condition, 
                              values = c(seq(1,tt[1]), seq(1,tt[2]),
                                         seq(1,tt[3]), seq(1,tt[4])))
    
    stimulus <- append(x = stimulus, values = st)
    
    stimulus_char <- append(x = stimulus_char, values = indx_stim[st])
    
    response <- append(x = response, values = rs)
    
    response_char <- append(x = response_char, values = cats[rs + 1])
    
    category <- append(x = category, values = ct - 1)
    
    category_char <- append(x = category_char, values = cats[ct])
    
    correct <- append(x = correct, 
                      values = ifelse(test = rs == (ct - 1), 
                                      yes = TRUE, no = FALSE))
  }
  
  out_tmp <- dplyr::tibble(id, trial, condition, trial_condition,
                           stimulus, stimulus_char, response, response_char, 
                           category, category_char, correct)
  if(length(conditions_keep) < 4){
    out_tmp <- subset(x = out_tmp, 
                      subset = out_tmp$condition %in% conditions_keep)
  }
  
  if(missing(file_suffix)){
    file_suffix <- "all"
  }
  
  readr::write_csv(x = out_tmp, 
                   file = paste(c("data/csv-files/lee-navarro-2002-", 
                                  file_suffix, ".csv"), collapse = ""))
}
