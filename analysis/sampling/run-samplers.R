################################################################################
## This code runs the code for the posterior sampling of the two models used  ##
################################################################################

# run sampler for interaction model
source(file = paste(c("analysis/sampling/model-parameters/",
                      "lee-navarro-type4-filtered-sampling-chmm.R"),
                    collapse = ""))

# run sampler for no-interaction model
source(file = 
         paste(c("analysis/sampling/model-parameters/",
                 "lee-navarro-type4-filtered-sampling-chmm-no-interaction.R"),
               collapse = ""))

# run sampler single stickiness and similarity weight
source(file = 
         paste(c("analysis/sampling/model-parameters/",
                 "lee-navarro-type4-filtered-chmm-stick.R"),
               collapse = ""))

# run sampler single stickiness and similarity weight
source(file = 
         paste(c("analysis/sampling/model-parameters/",
                 "lee-navarro-type4-filtered-chmm-stick-weight.R"),
               collapse = ""))


# run sampler single stickiness and similarity weight
source(file = 
         paste(c("analysis/sampling/model-parameters/",
                 "lee-navarro-type1-filtered-chmm-stick-weight.R"),
               collapse = ""))
