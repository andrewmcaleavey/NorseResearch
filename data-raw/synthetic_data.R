## code to prepare `synthetic_data` dataset goes here

# Keep the bundled example reproducible and NF2-only, while retaining the
# trigger-driven missingness used by the current generator.
set.seed(20260903)
synthetic_data <- random_norse_data(500, versions = "2") %>%
  score_all_NORSE2()

usethis::use_data(synthetic_data, overwrite = TRUE)
