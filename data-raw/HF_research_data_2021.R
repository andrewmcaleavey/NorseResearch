## code to prepare `HF_research_data_2021`
# this takes the existing data, removes the birthdate
# and should add a few convenience features

mh.strings <- c("dps", "DPS", "Dps",
                "PSK", "psk", "Psk", "pdk",
                "ISP", "isp", "Isp", "IPS",
                "Florø")

# substance use
sub.strings <- c("rus", "Rus", "RUS")

# inpt
inpt.strings <- c("døgn", "DØGN", "Døgn",
                  "NPS ruspost", "DPS Tronvik",
                  "NPS Ruspost")

# outpt
outpt.strings <- c("dag", "DAG", "Dag", "ISP", "ISP Sogndal", "Florø",
                   "DPS Florø", "DPS Sogndal")

HF_research_data_2021 <- NORSEpkg::hf.all.scored.2021.05.10 %>%
  # extract birthyear (birthdate removed later)
  mutate(birthyear = format(.data$respondent_birthdate, "%Y"),
         # add a 'date' variable for convenience
         date = assessment_instance_last_modified_submitted) %>%
  # arrange logically by patient and date
  arrange(respondent_id, date) %>%
  # remove rows that are not from routine care
  filter(!assessment_instance_context_label %in% c("pakke2", "Andre gang", "pakke1")) %>%

  # computing values per patient (not other groups)
  group_by(respondent_id) %>%
  mutate(pt_order = row_number(),
         pt_total_obs = n(),
         pt_first_date = min(.data$date),
         pt_wks_since_first = as.numeric(difftime(date,
                                        pt_first_date,
                                        units = "weeks")),
         pt_total_txs = length(unique(.data$treatment_id)),
         pt_tx_order = cumsum(!duplicated(.data$treatment_id))) %>%
  group_by(treatment_id) %>%
  mutate(tx_order = row_number(),
         tx_total = n(),
         tx_first_date = min(.data$date),
         tx_wks_since_first = as.numeric(difftime(date,
                                                  tx_first_date,
                                                  units = "weeks"))) %>%
  ungroup() %>%

  # add filters to categorize settings and treatment focus for each line of data
  mutate(is_MH = ifelse(str_detect(.data$treatment_name,
                                   paste(mh.strings,
                                         collapse = "|")),
                        TRUE,
                        FALSE),
         is_sub = ifelse(str_detect(.data$treatment_name,
                                    paste(sub.strings,
                                          collapse = "|")),
                         TRUE,
                         FALSE),
         is_inpt = ifelse(str_detect(.data$treatment_name,
                                     paste(inpt.strings,
                                           collapse = "|")),
                          TRUE,
                          FALSE),
         is_outpt = ifelse(str_detect(.data$treatment_name,
                                      paste(outpt.strings,
                                            collapse = "|")),
                           TRUE,
                           FALSE),
         # compute sum to determine how many categories each row is assigned.
         sum_tx_cats = is_MH + is_sub + is_inpt + is_outpt) %>%
  mutate(tx_focus = case_when(is_MH & !is_sub ~ "MH",
                              is_sub & !is_MH ~ "Sub",
                              is_MH & is_sub ~ "Sub",  # this says that if there is any use of "Rus" it is substance
                              TRUE ~ "Unclear"),
         in_or_out = case_when(is_inpt & !is_outpt ~ "Inpatient",
                               is_outpt & !is_inpt ~ "Outpatient",
                               is_outpt & is_inpt ~ "Both - unclear",
                               TRUE ~ "Unclear")) %>%

  # add variables for whether a patient has ANY substance treatment
  group_by(respondent_id) %>%
  mutate(pt_total_sub_tx = sum(is_sub),
         pt_total_MH_tx = sum(is_MH),
         pt_any_sub = ifelse(pt_total_sub_tx > 0, TRUE, FALSE),
         pt_any_MH = ifelse(pt_total_MH_tx > 0, TRUE, FALSE)) %>%

  # get ready for export by ungrouping and arranging variables
  ungroup() %>%
  select(respondent_id,
         treatment_id,
         date,
         pt_first_date,
         pt_wks_since_first,
         pt_order,
         pt_total_obs,
         pt_total_txs,
         pt_tx_order,
         tx_first_date,
         tx_wks_since_first,
         tx_order,
         tx_total,
         treatment_name,
         tx_focus,
         in_or_out,
         is_MH,
         is_sub,
         is_inpt,
         is_outpt,
         pt_any_MH,
         pt_any_sub,
         pt_total_MH_tx,
         pt_total_sub_tx,
         sum_tx_cats,
         birthyear,
         everything(),
         -respondent_birthdate)

# Data checks of interest:
#
# table(HF_research_data_2021$tx_focus)
# table(HF_research_data_2021$in_or_out)
# with(HF_research_data_2021, table(in_or_out, tx_focus))
#
# with(HF_research_data_2021 %>%
#        group_by(respondent_id) %>%
#        slice(1),
#      table(pt_any_MH))
#
# with(HF_research_data_2021 %>%
#        group_by(respondent_id) %>%
#        slice(1),
#      table(pt_any_sub))
#
#
# with(HF_research_data_2021 %>%
#        group_by(respondent_id) %>%
#        slice(1),
#      table(pt_any_MH, pt_any_sub))

usethis::use_data(HF_research_data_2021, overwrite = TRUE)
