## code to prepare `NF3.1_items` dataset goes here

NF3.1_items <- read.csv("data-raw/NF3.1_items.csv",
                        colClasses = c("CODE..prefixed.with.Q." = "character")) %>%
  NorseResearch::clean_NF_names() %>%
  select(-starts_with("X")) %>%
  mutate(assess_order = Order_in_Assessment,
         item = paste0("Q", CODE_prefixed_with_Q),
         reverse = Reverse_score,
         trigger = TriggerQ,
         simple_scale = "temp",
         item_text_e = English_item_question,
         scale_e = English_dimension_subscale,
         domain_e = English_domain,
         item_text_n = Nynorsk_item_question,
         scale_n = Nynorsk_dimension_subscale,
         domain_n = Nynorsk_domain,
         item_text_b = Bokm_l_item_question,
         scale_b = Bokm_l_dimension_subscale,
         domain_b = Bokm_l_domain,
         .before = 1,
         .keep = "unused") %>%
  mutate(simple_scale = case_when(scale_e == "Sad Affect" ~ "sad",
                                  scale_e == "Physical Anxiety" ~ "physAnx",
                                  scale_e == "Restrictive Eating" ~ "eating",
                                  scale_e == "Suicidal thoughts" ~ "suicide",
                                  scale_e == "Substance Use" ~ "subUse",
                                  scale_e == "Intrusive Memories" ~ "intMem",
                                  scale_e == "Anger" ~ "anger",
                                  scale_e == "Hopelessness" ~ "hopeless",
                                  scale_e == "Worry" ~ "worry",
                                  scale_e == "Self-Compassion" ~ "selfComp",
                                  scale_e == "Social Avoidance" ~ "socAvoid",
                                  scale_e == "Internal Avoidance" ~ "intAvoid",
                                  scale_e == "Self-Contempt" ~ "selfContempt",
                                  scale_e == "Pain" ~ "pain",
                                  scale_e == "Social Support" ~ "socSup",
                                  scale_e == "General Functioning" ~ "genFunc",
                                  scale_e == "Cognitive Problems" ~ "cog",
                                  scale_e == "Impulsivity" ~ "impulsivity",
                                  scale_e == "Readiness for Change" ~ "ready",
                                  scale_e == "Sexuality" ~ NA,
                                  scale_e == "Self-harm" ~ NA,
                                  scale_e == "Sleep" ~ NA,
                                  scale_e == "Physical health" ~ NA,
                                  scale_e == "Quality of Life" ~ NA,
                                  scale_e == "Finances" ~ NA,
                                  scale_e == "Health" ~ NA,
                                  scale_e == "Safety" ~ NA,
                                  scale_e == "Community" ~ NA,
                                  scale_e == "Alliance (Goal)" ~ "alliance",
                                  scale_e == "Alliance (Bond)" ~ "alliance",
                                  scale_e == "Alliance (Task)" ~ "alliance",
                                  scale_e == "Medication" ~ NA,
                                  scale_e == "Norse" ~ NA,
                                  scale_e == "Therapy Preferences" ~ "pref")) %>%
  # need to remove trailing .1 from item names
  mutate(item = str_remove(item, "\\.1$"))
# once simple_scale is taken care of, can delete rest of items

unique(NF3.1_items$scale_e)
# simple_scales_nf3 <- c()

usethis::use_data(NF3.1_items, overwrite = TRUE)
