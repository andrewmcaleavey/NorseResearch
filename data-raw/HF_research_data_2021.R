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

anon_ids <- tibble(NORSEpkg::hf.all.scored.2021.05.10) %>%
  dplyr::select(respondent_id) %>%
  distinct() %>%
  mutate(anon_id = order(respondent_id + runif(n(), min = -10000, max = 10000)))
# do same for treatment_id and assessment_instance_id
treatment_id_hash <- tibble(NORSEpkg::hf.all.scored.2021.05.10) %>%
  dplyr::select(treatment_id) %>%
  distinct() %>%
  mutate(anon_tx_id = order(treatment_id + runif(n(), min = -90000, max = 90000)))
assess_id_hash <- tibble(NORSEpkg::hf.all.scored.2021.05.10) %>%
  dplyr::select(assessment_instance_id) %>%
  distinct() %>%
  mutate(anon_assess_id = order(assessment_instance_id + runif(n(), min = -90000, max = 90000)))

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
         pt_any_MH = ifelse(pt_total_MH_tx > 0, TRUE, FALSE),
         cog_first_pt = first(cog, order_by = date),
         control_first_pt = first(control, order_by = date),
         eating_first_pt = first(eating, order_by = date),
         genFunc_first_pt = first(genFunc, order_by = date),
         hopeless_first_pt = first(hopeless, order_by = date),
         internal_first_pt = first(internal, order_by = date),
         irritable_first_pt = first(irritable, order_by = date),
         ready_first_pt = first(ready, order_by = date),
         recovEnv_first_pt = first(recovEnv, order_by = date),
         sad_first_pt = first(sad, order_by = date),
         selfCrit_first_pt = first(selfCrit, order_by = date),
         avoidSit_first_pt = first(avoidSit, order_by = date),
         avoidSoc_first_pt = first(avoidSoc, order_by = date),
         socialSafety_first_pt = first(socialSafety, order_by = date),
         somAnx_first_pt = first(somAnx, order_by = date),
         subRecov_first_pt = first(subRecov, order_by = date),
         subUse_first_pt = first(subUse, order_by = date),
         suicide_first_pt = first(suicide, order_by = date),
         trauma_first_pt = first(trauma, order_by = date),
         worry_first_pt = first(worry, order_by = date)) %>%

  ungroup() %>%
  # need to recode respondent_id and any other even potentially identifying variables.
  left_join(anon_ids) %>%
  left_join(treatment_id_hash) %>%
  left_join(assess_id_hash) %>%
  # get ready for export by ungrouping and arranging variables
  ungroup() %>%
  select(anon_id,
         anon_tx_id,
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
         anon_assess_id,
         everything(),
         -respondent_id,
         -assessment_instance_id,
         -treatment_id,
         -respondent_birthdate)

# Data checks of interest:
# length(unique(HF_research_data_2021$anon_id)) == length(unique(HF_research_data_2021$respondent_id))
# length(unique(HF_research_data_2021$anon_tx_id)) == length(unique(HF_research_data_2021$treatment_id))
# length(unique(HF_research_data_2021$anon_assess_id)) == length(unique(HF_research_data_2021$assessment_instance_id))
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
