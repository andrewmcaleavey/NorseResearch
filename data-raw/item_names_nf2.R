## code to prepare `item_names_nf2` dataset goes here

item_names_nf2 <- names(NORSEpkg::hf.scored.2019)[grepl("Q", names(NORSEpkg::hf.scored.2019))]

usethis::use_data(item_names_nf2, overwrite = TRUE)
