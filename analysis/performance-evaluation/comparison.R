# clean environment including hidden objects
rm(list = ls(all.names = TRUE))

# free RAM usage
gc()

# load moving average function 
source(file = "src/summary-f/moving-average.R")

# load posterior adequacy plotting function
source(file = "src/plot-f/plot-posterior-adequacy.R")

# load data
lee <- readr::read_csv(
  file = "data/csv-files/lee-navarro-2002-type4-filtered.csv")

# participants to remove from the analysis
participants_id <- unique(lee$id)[-c(which(unique(lee$id) == 6),
                                     which(unique(lee$id) == 22))]

# filter original data
lee_filt <- lee |>
  subset(subset = id %in% participants_id)

adequacy_full <- readRDS(
  file = paste(c("data/posterior-samples/posterior-adequacy/",
                 "lee-navarro-type4-filtered-chmm.rds"),
               collapse = ""))

adequacy_full <- adequacy_full[-c(which(unique(lee$id) == 6),
                                  which(unique(lee$id) == 22)), ]

adequacy_ni <- readRDS(
  file = paste(c("data/posterior-samples/posterior-adequacy/",
                 "lee-navarro-type4-filtered-chmm-no-interaction.rds"),
               collapse = ""))

adequacy_ni <- adequacy_ni[-c(which(unique(lee$id) == 6),
                              which(unique(lee$id) == 22)), ]


plot(0,0, xlim = c(0,98), ylim = c(-1,1), type = "n")

for (i in 1:dim(adequacy_full)[1]) {
  lines(x = seq(1, length(adequacy_full[i, which(!is.na(adequacy_full[i,]))])),
        y = adequacy_full[i, which(!is.na(adequacy_full[i, ]))] - 
            adequacy_ni[i, which(!is.na(adequacy_ni[i, ]))])
}



