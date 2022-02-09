## code to prepare `summary_norms_MH_out` dataset goes here
# this table is based on code found in "norms_reg.Rmd"
summary_norms_MH_out <- readRDS(file = "data-raw/summary_norms_MH_out.rds")

usethis::use_data(summary_norms_MH_out, overwrite = TRUE)
