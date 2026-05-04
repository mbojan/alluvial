library(dplyr)

Polpan <- here::here("data-raw", "polpan-voting-alluvial-plots.csv") |> 
  read.csv() |> 
  mutate(across(where(is.character), \(x) gsub(" \n", " ", x)))

usethis::use_data(Polpan)
