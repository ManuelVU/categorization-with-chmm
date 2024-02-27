# load sampling functions
prefix <- paste(c(here::here(),"/src/sampling-f/three-parameters/"), collapse = "")

files_sources <- list.files(path = prefix)

files_sources <- cbind(rep(x = prefix, times = length(files_sources)),
                       files_sources) |> 
  apply(MARGIN = 1, FUN = paste, collapse = "")

files_sources <- files_sources[stringr::str_detect(string = files_sources, pattern = ".R")]

sapply(files_sources, source)

# load similarity functions
prefix <- paste(c(here::here(),"/src/similarity-f/"), collapse = "")
files_sources <- list.files(path = prefix)
files_sources <- cbind(rep(x = prefix, times = length(files_sources)),
                       files_sources) |> 
  apply(MARGIN = 1, FUN = paste, collapse = "")

sapply(files_sources, source)