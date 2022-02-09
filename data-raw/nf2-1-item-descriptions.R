## code to prepare `nf2.1.item.descriptions` dataset goes here

# This is an update to item_descriptions, for NF v.2.1.
# starting to add on 7 Sep 2020.

require(tidyverse)
nf2.1.item.descriptions <- readxl::read_xlsx("data-raw/NF2.1.items.xlsx") %>%
  mutate(reverse = if_else(is.na(.data$reverse),
                           FALSE,
                           TRUE),
         trigger = if_else(is.na(.data$trigger),
                           FALSE,
                           TRUE))

usethis::use_data(nf2.1.item.descriptions, overwrite = TRUE)
