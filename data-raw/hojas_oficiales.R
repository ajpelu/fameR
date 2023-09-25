## code to prepare `hojas_oficiales` dataset goes here


library(dplyr)
hojas_validas <- 'data-raw/hojas_oficiales.csv' |> read.csv() |> pull()


usethis::use_data(hojas_oficiales, overwrite = TRUE)
