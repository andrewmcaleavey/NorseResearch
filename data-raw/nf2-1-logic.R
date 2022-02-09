## code to prepare `nf2.1.logic` dataset goes here

library(readxl)
nf2.1.logic <- readxl::read_xlsx("data-raw/NF2.1.logic.xlsx")

usethis::use_data(nf2.1.logic, overwrite = TRUE)
