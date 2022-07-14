## code to prepare `neighbour_info` dataset goes here

neighbour_info <- read.csv('data-raw/neighbour_info.csv')

usethis::use_data(neighbour_info, overwrite = TRUE)
