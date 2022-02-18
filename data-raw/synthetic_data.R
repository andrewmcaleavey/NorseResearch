## code to prepare `synthetic_data` dataset goes here

synthetic_data <- random_norse_data(500) %>%
  NORSEpkg::score_all_NORSE2()

usethis::use_data(synthetic_data, overwrite = TRUE)
