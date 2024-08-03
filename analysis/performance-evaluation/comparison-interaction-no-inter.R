################################################################################
## Comparison between interaction and no interaction CHMM                     ##
################################################################################

# clean environment including hidden objects
rm(list = ls(all.names = TRUE))

# free RAM usage
gc()

# load posterior adequacy samples for interaction model
stick_weight <- readRDS(
  file = paste(c("data/posterior-samples/posterior-adequacy/",
                 "lee-navarro-type4-filtered-chmm-stick-weight.rds"),
               collapse = ""))

# load posterior adequacy samples for no interaction model
stick <- readRDS(
  file = paste(c("data/posterior-samples/posterior-adequacy/",
                 "lee-navarro-type4-filtered-chmm-stick.rds"),
               collapse = ""))

plot(0,0,xlim = c(0,100), ylim = c(-0.5,0.5), type = "n")
for(i in 1:dim(stick)[1]){
  lines(x = seq(1:length(which(!is.na(stick[i,])))), 
        y = (stick_weight[i, 1:max(which(!is.na(stick[i,])))] - 
               stick[i, 1:max(which(!is.na(stick[i,])))]))
}
