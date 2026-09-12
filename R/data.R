### DATA DOCUMENTATION


#' Item information, NF 2.1
#'
#' A summary of information about the NF items, updated September 2020
#' @format a data frame
#'   \describe{
#'     \item{\code{assess_order}}{Numeric. Order in assessment.}
#'     \item{\code{item}}{Character. Item name (e.g., "Q14").}
#'     \item{\code{reverse}}{Logical. Is item reverse scored?}
#'     \item{\code{trigger}}{Logical. Is the item used as an opening trigger?}
#'     \item{\code{item_text_e}}{Character. Text of the item in English.}
#'     \item{\code{simple_scale}}{Character. Scale name for programming.}
#'     \item{\code{scale_e}}{Character. Scale name for presentation.}
#'     \item{\code{domain_e}}{Character. Domain name in English.}
#'     \item{\code{item_text_n}}{Character. Text of the item in Nynorsk.}
#'     \item{\code{scale_n}}{Character. Scale name in Nynorsk.}
#'     \item{\code{domain_n}}{Character. Domain name in Nynorsk.}
#'     \item{\code{item_text_b}}{Character. Text of the item in Bokmål.}
#'     \item{\code{scale_b}}{Character. Scale name in Bokmål.}
#'     \item{\code{domain_b}}{Character. Domain name in Bokmål.}
#'   }
#' @details Not generally accessed directly. Use lookup functions like \code{\link{lookup_item}}.
#'
#' @source Based on 'NORSE Measure Master Document.xlsx', and more proximally 'NF2.1.items.xlsx'.
"nf2.1.item.descriptions"

#' NF v. 2.1 Logic
#'
#' A table to summarize opening and closing logic in NF 2.1
#' Added 7 September 2020.
#' @format A data frame.
#'   \describe{
#'     \item{\code{simple_scale}}{Character. Scale name for programming.}
#'     \item{\code{scale_e}}{Character. Scale name for presentation.}
#'     \item{\code{scale_n}}{Character. Scale name in Nynorsk.}
#'     \item{\code{trigger_item}}{Character. Item name (e.g., "Q145") of the trigger for this scale.}
#'     \item{\code{trigger_val}}{Numeric. Value of the opening threshold. Item response
#'     must be greater than this to open the scale.}
#'     \item{\code{closing_threshold}}{Numeric. Value of the closing threshold.
#'     Moving average of past 3 administrations must be below this value to close
#'     the scale.}
#'   }
#' @details Not generally accessed directly. Use lookup functions like \code{\link{lookup_item}}.
#' Used in scoring.
#'
#' @source Based on 'NORSE Measure Master Document.xlsx', and more proximally 'NF2.1.logic.xlsx'.
"nf2.1.logic"


#' Summary norm data for mental health specialty clinics, NF2, Helse Førde data, from 2019.
#' Only includes mean, SD, and N.
#'
#' @format a table.
#'   \describe{
#'     \item{\code{var}}{Variable in each row. Values: Mean, SD, and N}
#'     \item{\code{cog}}{Cognitive problems}
#'     \item{\code{ALLOTHERS}}{Other scales in NF2.1}
#'   }
#'
"summary_norms_MH_out"


#' Research-friendly data from Helse Førde 2021.
#'
#' Anonymous research data from Helse Førde area (all clinics)
#' from May 2021, scored, with additional variables for external use.
#'
#' Any values that were 99 are now NA, and these represent patient non-responses.
#'
#' THIS DATA IS NOT PROVIDED as part of the package and must be received separately
#' for confidentiality. Once received, the .rda file can be saved in the
#' global environment as \code{HF_research_data_2021 <- load("HF_research_data_2021.rda")}.
#' This process means that it is not really accessible as part of the package,
#' but must be stored locally.
#'
#' The codebook for the relevant items is provided here though.
#'
#' @format A data frame with 18620 observations of 190 variables.
#' \describe{
#'     \item{\code{anon_id}}{Numeric. Unique patient number within this data only, anonymized.}
#'     \item{\code{anon_tx_id}}{Numeric. Unique treatment number, anonymized.}
#'     \item{\code{date}}{Date. Date (and time) Norse Feedback was completed.}
#'     \item{\code{pt_first_date}}{Date. First date in the data for each respondent.}
#'     \item{\code{pt_wks_since_first}}{Numeric. Time difference in weeks from the patient's first date.}
#'     \item{\code{pt_order}}{Numeric. Observation number within patient across treatment types.}
#'     \item{\code{pt_total_obs}}{Numeric. Total number of observations for this patient.}
#'     \item{\code{pt_total_txs}}{Numeric. Total number of treatments for this patient.}
#'     \item{\code{pt_tx_order}}{Numeric. Treatment number within patient.}
#'     \item{\code{tx_first_date}}{Date. First date for this treatment.}
#'     \item{\code{tx_wks_since_first}}{Numeric. Time difference in weeks from the treatment's first date.}
#'     \item{\code{tx_order}}{Numeric. Observation number within treatment and respondent.}
#'     \item{\code{tx_total}}{Numeric. Total number of observations in this treatment.}
#'     \item{\code{treatment_name}}{Character. Treatment type or location as recorded in the source data.}
#'     \item{\code{tx_focus}}{Character. Treatment focus: Substance use, mental health, or unclear.}
#'     \item{\code{in_or_out}}{Character. Whether the treatment is inpatient, outpatient, or unclear.}
#'     \item{\code{is_MH}}{Logical. Whether the treatment is a mental-health service.}
#'     \item{\code{is_sub}}{Logical. Whether the treatment is a substance-use service.}
#'     \item{\code{is_inpt}}{Logical. Whether the treatment is an inpatient service.}
#'     \item{\code{is_outpt}}{Logical. Whether the treatment is an outpatient service.}
#'     \item{\code{pt_any_MH}}{Logical. Whether the patient has any mental-health service observations.}
#'     \item{\code{pt_any_sub}}{Logical. Whether the patient has any substance-use service observations.}
#'     \item{\code{pt_total_MH_tx}}{Numeric. Total number of mental-health service observations for this patient.}
#'     \item{\code{pt_total_sub_tx}}{Numeric. Total number of substance-use service observations for this patient.}
#'     \item{\code{sum_tx_cats}}{Numeric. Number of treatment categories assigned to the treatment name.}
#'     \item{\code{birthyear}}{Character. Year of birth for each respondent.}
#'     \item{\code{anon_assess_id}}{Numeric. Unique anonymized assessment-instance identifier.}
#'     \item{\code{cog}}{Numeric. Score for the Cognitive problems/concentration problems scale.}
#'     \item{\code{control}}{Numeric. Score for the Control scale.}
#'     \item{\code{eating}}{Numeric. Score for the Eating problems scale.}
#'     \item{\code{genFunc}}{Numeric. Score for the General functioning scale.}
#'     \item{\code{hopeless}}{Numeric. Score for the Hopelessness/demoralization scale.}
#'     \item{\code{internal}}{Numeric. Score for the Internal avoidance scale.}
#'     \item{\code{irritable}}{Numeric. Score for the Irritability scale.}
#'     \item{\code{ready}}{Numeric. Score for the Readiness for recovery scale.}
#'     \item{\code{recovEnv}}{Numeric. Score for the Recovery environment scale.}
#'     \item{\code{sad}}{Numeric. Score for the Sadness/affect scale.}
#'     \item{\code{selfCrit}}{Numeric. Score for the Self-criticism scale.}
#'     \item{\code{avoidSit}}{Numeric. Score for the Situational avoidance scale.}
#'     \item{\code{avoidSoc}}{Numeric. Score for the Social avoidance scale.}
#'     \item{\code{socialSafety}}{Numeric. Score for the Social safety scale.}
#'     \item{\code{somAnx}}{Numeric. Score for the Somatic anxiety scale.}
#'     \item{\code{subRecov}}{Numeric. Score for the Substance recovery scale.}
#'     \item{\code{subUse}}{Numeric. Score for the Substance use scale.}
#'     \item{\code{suicide}}{Numeric. Score for the Suicide scale.}
#'     \item{\code{trauma}}{Numeric. Score for the Trauma reaction scale.}
#'     \item{\code{worry}}{Numeric. Score for the Worry scale.}
#'     \item{\code{cog_first_pt}}{Numeric. First observed Cognitive problems/concentration problems score for this patient.}
#'     \item{\code{control_first_pt}}{Numeric. First observed Control score for this patient.}
#'     \item{\code{eating_first_pt}}{Numeric. First observed Eating problems score for this patient.}
#'     \item{\code{genFunc_first_pt}}{Numeric. First observed General functioning score for this patient.}
#'     \item{\code{hopeless_first_pt}}{Numeric. First observed Hopelessness/demoralization score for this patient.}
#'     \item{\code{internal_first_pt}}{Numeric. First observed Internal avoidance score for this patient.}
#'     \item{\code{irritable_first_pt}}{Numeric. First observed Irritability score for this patient.}
#'     \item{\code{ready_first_pt}}{Numeric. First observed Readiness for recovery score for this patient.}
#'     \item{\code{recovEnv_first_pt}}{Numeric. First observed Recovery environment score for this patient.}
#'     \item{\code{sad_first_pt}}{Numeric. First observed Sadness/affect score for this patient.}
#'     \item{\code{selfCrit_first_pt}}{Numeric. First observed Self-criticism score for this patient.}
#'     \item{\code{avoidSit_first_pt}}{Numeric. First observed Situational avoidance score for this patient.}
#'     \item{\code{avoidSoc_first_pt}}{Numeric. First observed Social avoidance score for this patient.}
#'     \item{\code{socialSafety_first_pt}}{Numeric. First observed Social safety score for this patient.}
#'     \item{\code{somAnx_first_pt}}{Numeric. First observed Somatic anxiety score for this patient.}
#'     \item{\code{subRecov_first_pt}}{Numeric. First observed Substance recovery score for this patient.}
#'     \item{\code{subUse_first_pt}}{Numeric. First observed Substance use score for this patient.}
#'     \item{\code{suicide_first_pt}}{Numeric. First observed Suicide score for this patient.}
#'     \item{\code{trauma_first_pt}}{Numeric. First observed Trauma reaction score for this patient.}
#'     \item{\code{worry_first_pt}}{Numeric. First observed Worry score for this patient.}
#'     \item{\code{assessment_version}}{Character. Norse Feedback back-end assessment version.}
#'     \item{\code{assessment_instance_title}}{Character. Assessment-instance title.}
#'     \item{\code{assessment_instance_start_date}}{Date. Assessment-instance start date.}
#'     \item{\code{assessment_instance_end_date}}{Date. Assessment-instance end date.}
#'     \item{\code{assessment_instance_created_date}}{Date. Assessment-instance creation date.}
#'     \item{\code{assessment_instance_last_modified_submitted}}{Date. Date the assessment instance was last modified or submitted.}
#'     \item{\code{assessment_instance_has_started}}{Logical. Whether the assessment instance has started.}
#'     \item{\code{assessment_instance_is_submitted}}{Logical. Whether the assessment instance was submitted.}
#'     \item{\code{assessment_instance_is_closed}}{Logical. Whether the assessment instance was closed.}
#'     \item{\code{assessment_instance_context_label}}{Character. Assessment context label, such as intake, weekly assessment, or termination.}
#'     \item{\code{assessment_instance_first_time_started_date}}{Date. First time the assessment instance was started.}
#'     \item{\code{assessment_instance_first_time_submitted_date}}{Date. First time the assessment instance was submitted.}
#'     \item{\code{treatment_type_id}}{Numeric. Treatment-type identifier.}
#'     \item{\code{treatment_type_name}}{Character. Treatment-type name.}
#'     \item{\code{respondent_gender}}{Character. Respondent gender.}
#'     \item{\code{respondent_account_enabled}}{Numeric. Source-system indicator that the respondent account is enabled.}
#'     \item{\code{respondent_test_account}}{Numeric. Source-system indicator that the respondent account is a test account.}
#'     \item{\code{respondent_last_login}}{Date. Respondent's last login date.}
#'     \item{\code{respondent_communication_disabled}}{Numeric. Source-system indicator that respondent communications are disabled.}
#'     \item{\code{Q42}}{Numeric. Response to Norse Feedback item Q42.}
#'     \item{\code{Q51}}{Numeric. Response to Norse Feedback item Q51.}
#'     \item{\code{Q46}}{Numeric. Response to Norse Feedback item Q46.}
#'     \item{\code{Q19}}{Numeric. Response to Norse Feedback item Q19.}
#'     \item{\code{Q59}}{Numeric. Response to Norse Feedback item Q59.}
#'     \item{\code{Q111}}{Numeric. Response to Norse Feedback item Q111.}
#'     \item{\code{Q37}}{Numeric. Response to Norse Feedback item Q37.}
#'     \item{\code{Q61}}{Numeric. Response to Norse Feedback item Q61.}
#'     \item{\code{Q39}}{Numeric. Response to Norse Feedback item Q39.}
#'     \item{\code{Q119}}{Numeric. Response to Norse Feedback item Q119.}
#'     \item{\code{Q64}}{Numeric. Response to Norse Feedback item Q64.}
#'     \item{\code{Q34}}{Numeric. Response to Norse Feedback item Q34.}
#'     \item{\code{Q38}}{Numeric. Response to Norse Feedback item Q38.}
#'     \item{\code{Q26}}{Numeric. Response to Norse Feedback item Q26.}
#'     \item{\code{Q43}}{Numeric. Response to Norse Feedback item Q43.}
#'     \item{\code{Q135}}{Numeric. Response to Norse Feedback item Q135.}
#'     \item{\code{Q80}}{Numeric. Response to Norse Feedback item Q80.}
#'     \item{\code{Q27}}{Numeric. Response to Norse Feedback item Q27.}
#'     \item{\code{Q146}}{Numeric. Response to Norse Feedback item Q146.}
#'     \item{\code{Q110}}{Numeric. Response to Norse Feedback item Q110.}
#'     \item{\code{Q103}}{Numeric. Response to Norse Feedback item Q103.}
#'     \item{\code{Q4}}{Numeric. Response to Norse Feedback item Q4.}
#'     \item{\code{Q126}}{Numeric. Response to Norse Feedback item Q126.}
#'     \item{\code{Q75}}{Numeric. Response to Norse Feedback item Q75.}
#'     \item{\code{Q107}}{Numeric. Response to Norse Feedback item Q107.}
#'     \item{\code{Q3}}{Numeric. Response to Norse Feedback item Q3.}
#'     \item{\code{Q113}}{Numeric. Response to Norse Feedback item Q113.}
#'     \item{\code{Q121}}{Numeric. Response to Norse Feedback item Q121.}
#'     \item{\code{Q53}}{Numeric. Response to Norse Feedback item Q53.}
#'     \item{\code{Q11}}{Numeric. Response to Norse Feedback item Q11.}
#'     \item{\code{Q12}}{Numeric. Response to Norse Feedback item Q12.}
#'     \item{\code{Q13}}{Numeric. Response to Norse Feedback item Q13.}
#'     \item{\code{Q14}}{Numeric. Response to Norse Feedback item Q14.}
#'     \item{\code{Q15}}{Numeric. Response to Norse Feedback item Q15.}
#'     \item{\code{Q84}}{Numeric. Response to Norse Feedback item Q84.}
#'     \item{\code{Q149}}{Numeric. Response to Norse Feedback item Q149.}
#'     \item{\code{Q150}}{Numeric. Response to Norse Feedback item Q150.}
#'     \item{\code{Q151}}{Numeric. Response to Norse Feedback item Q151.}
#'     \item{\code{Q78}}{Numeric. Response to Norse Feedback item Q78.}
#'     \item{\code{Q115}}{Numeric. Response to Norse Feedback item Q115.}
#'     \item{\code{Q120}}{Numeric. Response to Norse Feedback item Q120.}
#'     \item{\code{Q20}}{Numeric. Response to Norse Feedback item Q20.}
#'     \item{\code{Q136}}{Numeric. Response to Norse Feedback item Q136.}
#'     \item{\code{Q35}}{Numeric. Response to Norse Feedback item Q35.}
#'     \item{\code{Q122}}{Numeric. Response to Norse Feedback item Q122.}
#'     \item{\code{Q131}}{Numeric. Response to Norse Feedback item Q131.}
#'     \item{\code{Q24}}{Numeric. Response to Norse Feedback item Q24.}
#'     \item{\code{Q138}}{Numeric. Response to Norse Feedback item Q138.}
#'     \item{\code{Q40}}{Numeric. Response to Norse Feedback item Q40.}
#'     \item{\code{Q67}}{Numeric. Response to Norse Feedback item Q67.}
#'     \item{\code{Q124}}{Numeric. Response to Norse Feedback item Q124.}
#'     \item{\code{Q145}}{Numeric. Response to Norse Feedback item Q145.}
#'     \item{\code{Q139}}{Numeric. Response to Norse Feedback item Q139.}
#'     \item{\code{Q143}}{Numeric. Response to Norse Feedback item Q143.}
#'     \item{\code{Q105}}{Numeric. Response to Norse Feedback item Q105.}
#'     \item{\code{Q77}}{Numeric. Response to Norse Feedback item Q77.}
#'     \item{\code{Q104}}{Numeric. Response to Norse Feedback item Q104.}
#'     \item{\code{Q144}}{Numeric. Response to Norse Feedback item Q144.}
#'     \item{\code{Q114}}{Numeric. Response to Norse Feedback item Q114.}
#'     \item{\code{Q57}}{Numeric. Response to Norse Feedback item Q57.}
#'     \item{\code{Q142}}{Numeric. Response to Norse Feedback item Q142.}
#'     \item{\code{Q116}}{Numeric. Response to Norse Feedback item Q116.}
#'     \item{\code{Q123}}{Numeric. Response to Norse Feedback item Q123.}
#'     \item{\code{Q129}}{Numeric. Response to Norse Feedback item Q129.}
#'     \item{\code{Q17}}{Numeric. Response to Norse Feedback item Q17.}
#'     \item{\code{Q140}}{Numeric. Response to Norse Feedback item Q140.}
#'     \item{\code{Q88}}{Numeric. Response to Norse Feedback item Q88.}
#'     \item{\code{Q117}}{Numeric. Response to Norse Feedback item Q117.}
#'     \item{\code{Q127}}{Numeric. Response to Norse Feedback item Q127.}
#'     \item{\code{Q68}}{Numeric. Response to Norse Feedback item Q68.}
#'     \item{\code{Q65}}{Numeric. Response to Norse Feedback item Q65.}
#'     \item{\code{Q102}}{Numeric. Response to Norse Feedback item Q102.}
#'     \item{\code{Q130}}{Numeric. Response to Norse Feedback item Q130.}
#'     \item{\code{Q100}}{Numeric. Response to Norse Feedback item Q100.}
#'     \item{\code{Q18}}{Numeric. Response to Norse Feedback item Q18.}
#'     \item{\code{Q101}}{Numeric. Response to Norse Feedback item Q101.}
#'     \item{\code{Q147}}{Numeric. Response to Norse Feedback item Q147.}
#'     \item{\code{Q106}}{Numeric. Response to Norse Feedback item Q106.}
#'     \item{\code{Q141}}{Numeric. Response to Norse Feedback item Q141.}
#'     \item{\code{Q63}}{Numeric. Response to Norse Feedback item Q63.}
#'     \item{\code{Q109}}{Numeric. Response to Norse Feedback item Q109.}
#'     \item{\code{Q128}}{Numeric. Response to Norse Feedback item Q128.}
#'     \item{\code{Q132}}{Numeric. Response to Norse Feedback item Q132.}
#'     \item{\code{Q71}}{Numeric. Response to Norse Feedback item Q71.}
#'     \item{\code{Q72}}{Numeric. Response to Norse Feedback item Q72.}
#'     \item{\code{Q152}}{Numeric. Response to Norse Feedback item Q152.}
#'     \item{\code{Q153}}{Numeric. Response to Norse Feedback item Q153.}
#'     \item{\code{Q74}}{Numeric. Response to Norse Feedback item Q74.}
#'     \item{\code{Q154}}{Numeric. Response to Norse Feedback item Q154.}
#'     \item{\code{Q62}}{Numeric. Response to Norse Feedback item Q62.}
#'     \item{\code{Q50}}{Numeric. Response to Norse Feedback item Q50.}
#'     \item{\code{Q112}}{Numeric. Response to Norse Feedback item Q112.}
#'     \item{\code{Q108}}{Numeric. Response to Norse Feedback item Q108.}
#'     \item{\code{Q137}}{Numeric. Response to Norse Feedback item Q137.}
#'     \item{\code{Q118}}{Numeric. Response to Norse Feedback item Q118.}
#'     \item{\code{Q155}}{Numeric. Response to Norse Feedback item Q155.}
#'     \item{\code{Q134}}{Numeric. Response to Norse Feedback item Q134.}
#'     \item{\code{Q133}}{Numeric. Response to Norse Feedback item Q133.}
#'     \item{\code{Q10}}{Numeric. Response to Norse Feedback item Q10.}
#'     \item{\code{Q82}}{Numeric. Response to Norse Feedback item Q82.}
#'     \item{\code{Q83}}{Numeric. Response to Norse Feedback item Q83.}
#'     \item{\code{Q148}}{Numeric. Response to Norse Feedback item Q148.}
#'     \item{\code{alliance}}{Numeric. Mean score across the alliance items.}
#'     \item{\code{needs}}{Numeric. Mean score across the needs items.}
#' }
#'
#' @source Processed from NORSEpkg::hf.all.scored.2021.05.10.
#' See \code{data-raw/HF_research_data_2021.R}.
"HF_research_data_2021"


#' Synthetic NF2 Data
#'
#' A reproducible NF2-only example generated by [random_norse_data()]. It uses
#' the NF2 trigger rules: items beyond a scale's trigger are `NA` when that
#' scale is not administered. Scale-score columns are included for examples.
#'
#' @format A data frame with 2888 observations of 133 variables.
#' \describe{
#'     \item{\code{anon_id}}{Numeric. Unique patient number within this data only, anonymized.}
#'     \item{\code{date}}{Date. Date (and time) Norse Feedback was completed.}
#'     \item{\code{pt_first_date}}{Date. First date in the data for each respondent.}
#'     \item{\code{pt_total_obs}}{Numeric. Total number of observations for this patient.}
#'     \item{\code{tx_focus}}{Character. Treatment focus: Substance use, mental health, or unclear.}
#'     \item{\code{in_or_out}}{Character. Whether the treatment is inpatient, outpatient, or unclear.}
#'     \item{\code{gender}}{Character. Respondent gender.}
#'     \item{\code{birthyear}}{Character. Year of birth for each respondent.}
#'     \item{\code{Ver_10}}{Character. Norse Feedback version identifier.}
#'     \item{\code{Q11}}{Numeric. Response to Norse Feedback item Q11.}
#'     \item{\code{Q12}}{Numeric. Response to Norse Feedback item Q12.}
#'     \item{\code{Q13}}{Numeric. Response to Norse Feedback item Q13.}
#'     \item{\code{Q14}}{Numeric. Response to Norse Feedback item Q14.}
#'     \item{\code{Q145}}{Numeric. Response to Norse Feedback item Q145.}
#'     \item{\code{Q146}}{Numeric. Response to Norse Feedback item Q146.}
#'     \item{\code{Q143}}{Numeric. Response to Norse Feedback item Q143.}
#'     \item{\code{Q144}}{Numeric. Response to Norse Feedback item Q144.}
#'     \item{\code{Q142}}{Numeric. Response to Norse Feedback item Q142.}
#'     \item{\code{Q147}}{Numeric. Response to Norse Feedback item Q147.}
#'     \item{\code{Q26}}{Numeric. Response to Norse Feedback item Q26.}
#'     \item{\code{Q20}}{Numeric. Response to Norse Feedback item Q20.}
#'     \item{\code{Q68}}{Numeric. Response to Norse Feedback item Q68.}
#'     \item{\code{Q130}}{Numeric. Response to Norse Feedback item Q130.}
#'     \item{\code{Q63}}{Numeric. Response to Norse Feedback item Q63.}
#'     \item{\code{Q104}}{Numeric. Response to Norse Feedback item Q104.}
#'     \item{\code{Q57}}{Numeric. Response to Norse Feedback item Q57.}
#'     \item{\code{Q18}}{Numeric. Response to Norse Feedback item Q18.}
#'     \item{\code{Q46}}{Numeric. Response to Norse Feedback item Q46.}
#'     \item{\code{Q140}}{Numeric. Response to Norse Feedback item Q140.}
#'     \item{\code{Q27}}{Numeric. Response to Norse Feedback item Q27.}
#'     \item{\code{Q141}}{Numeric. Response to Norse Feedback item Q141.}
#'     \item{\code{Q115}}{Numeric. Response to Norse Feedback item Q115.}
#'     \item{\code{Q15}}{Numeric. Response to Norse Feedback item Q15.}
#'     \item{\code{Q61}}{Numeric. Response to Norse Feedback item Q61.}
#'     \item{\code{Q24}}{Numeric. Response to Norse Feedback item Q24.}
#'     \item{\code{Q88}}{Numeric. Response to Norse Feedback item Q88.}
#'     \item{\code{Q34}}{Numeric. Response to Norse Feedback item Q34.}
#'     \item{\code{Q78}}{Numeric. Response to Norse Feedback item Q78.}
#'     \item{\code{Q122}}{Numeric. Response to Norse Feedback item Q122.}
#'     \item{\code{Q123}}{Numeric. Response to Norse Feedback item Q123.}
#'     \item{\code{Q10}}{Numeric. Response to Norse Feedback item Q10.}
#'     \item{\code{Q65}}{Numeric. Response to Norse Feedback item Q65.}
#'     \item{\code{Q114}}{Numeric. Response to Norse Feedback item Q114.}
#'     \item{\code{Q37}}{Numeric. Response to Norse Feedback item Q37.}
#'     \item{\code{Q82}}{Numeric. Response to Norse Feedback item Q82.}
#'     \item{\code{Q83}}{Numeric. Response to Norse Feedback item Q83.}
#'     \item{\code{Q133}}{Numeric. Response to Norse Feedback item Q133.}
#'     \item{\code{Q134}}{Numeric. Response to Norse Feedback item Q134.}
#'     \item{\code{Q135}}{Numeric. Response to Norse Feedback item Q135.}
#'     \item{\code{Q136}}{Numeric. Response to Norse Feedback item Q136.}
#'     \item{\code{Q80}}{Numeric. Response to Norse Feedback item Q80.}
#'     \item{\code{Q138}}{Numeric. Response to Norse Feedback item Q138.}
#'     \item{\code{Q139}}{Numeric. Response to Norse Feedback item Q139.}
#'     \item{\code{Q137}}{Numeric. Response to Norse Feedback item Q137.}
#'     \item{\code{Q100}}{Numeric. Response to Norse Feedback item Q100.}
#'     \item{\code{Q102}}{Numeric. Response to Norse Feedback item Q102.}
#'     \item{\code{Q42}}{Numeric. Response to Norse Feedback item Q42.}
#'     \item{\code{Q128}}{Numeric. Response to Norse Feedback item Q128.}
#'     \item{\code{Q126}}{Numeric. Response to Norse Feedback item Q126.}
#'     \item{\code{Q67}}{Numeric. Response to Norse Feedback item Q67.}
#'     \item{\code{Q124}}{Numeric. Response to Norse Feedback item Q124.}
#'     \item{\code{Q129}}{Numeric. Response to Norse Feedback item Q129.}
#'     \item{\code{Q127}}{Numeric. Response to Norse Feedback item Q127.}
#'     \item{\code{Q101}}{Numeric. Response to Norse Feedback item Q101.}
#'     \item{\code{Q38}}{Numeric. Response to Norse Feedback item Q38.}
#'     \item{\code{Q84}}{Numeric. Response to Norse Feedback item Q84.}
#'     \item{\code{Q149}}{Numeric. Response to Norse Feedback item Q149.}
#'     \item{\code{Q150}}{Numeric. Response to Norse Feedback item Q150.}
#'     \item{\code{Q151}}{Numeric. Response to Norse Feedback item Q151.}
#'     \item{\code{Q148}}{Numeric. Response to Norse Feedback item Q148.}
#'     \item{\code{Q118}}{Numeric. Response to Norse Feedback item Q118.}
#'     \item{\code{Q17}}{Numeric. Response to Norse Feedback item Q17.}
#'     \item{\code{Q119}}{Numeric. Response to Norse Feedback item Q119.}
#'     \item{\code{Q64}}{Numeric. Response to Norse Feedback item Q64.}
#'     \item{\code{Q121}}{Numeric. Response to Norse Feedback item Q121.}
#'     \item{\code{Q120}}{Numeric. Response to Norse Feedback item Q120.}
#'     \item{\code{Q43}}{Numeric. Response to Norse Feedback item Q43.}
#'     \item{\code{Q131}}{Numeric. Response to Norse Feedback item Q131.}
#'     \item{\code{Q40}}{Numeric. Response to Norse Feedback item Q40.}
#'     \item{\code{Q132}}{Numeric. Response to Norse Feedback item Q132.}
#'     \item{\code{Q62}}{Numeric. Response to Norse Feedback item Q62.}
#'     \item{\code{Q50}}{Numeric. Response to Norse Feedback item Q50.}
#'     \item{\code{Q51}}{Numeric. Response to Norse Feedback item Q51.}
#'     \item{\code{Q103}}{Numeric. Response to Norse Feedback item Q103.}
#'     \item{\code{Q75}}{Numeric. Response to Norse Feedback item Q75.}
#'     \item{\code{Q3}}{Numeric. Response to Norse Feedback item Q3.}
#'     \item{\code{Q53}}{Numeric. Response to Norse Feedback item Q53.}
#'     \item{\code{Q109}}{Numeric. Response to Norse Feedback item Q109.}
#'     \item{\code{Q154}}{Numeric. Response to Norse Feedback item Q154.}
#'     \item{\code{Q108}}{Numeric. Response to Norse Feedback item Q108.}
#'     \item{\code{Q155}}{Numeric. Response to Norse Feedback item Q155.}
#'     \item{\code{Q59}}{Numeric. Response to Norse Feedback item Q59.}
#'     \item{\code{Q4}}{Numeric. Response to Norse Feedback item Q4.}
#'     \item{\code{Q107}}{Numeric. Response to Norse Feedback item Q107.}
#'     \item{\code{Q35}}{Numeric. Response to Norse Feedback item Q35.}
#'     \item{\code{Q19}}{Numeric. Response to Norse Feedback item Q19.}
#'     \item{\code{Q105}}{Numeric. Response to Norse Feedback item Q105.}
#'     \item{\code{Q77}}{Numeric. Response to Norse Feedback item Q77.}
#'     \item{\code{Q106}}{Numeric. Response to Norse Feedback item Q106.}
#'     \item{\code{Q71}}{Numeric. Response to Norse Feedback item Q71.}
#'     \item{\code{Q72}}{Numeric. Response to Norse Feedback item Q72.}
#'     \item{\code{Q152}}{Numeric. Response to Norse Feedback item Q152.}
#'     \item{\code{Q153}}{Numeric. Response to Norse Feedback item Q153.}
#'     \item{\code{Q74}}{Numeric. Response to Norse Feedback item Q74.}
#'     \item{\code{Q111}}{Numeric. Response to Norse Feedback item Q111.}
#'     \item{\code{Q110}}{Numeric. Response to Norse Feedback item Q110.}
#'     \item{\code{Q113}}{Numeric. Response to Norse Feedback item Q113.}
#'     \item{\code{Q112}}{Numeric. Response to Norse Feedback item Q112.}
#'     \item{\code{Q117}}{Numeric. Response to Norse Feedback item Q117.}
#'     \item{\code{Q116}}{Numeric. Response to Norse Feedback item Q116.}
#'     \item{\code{Q39}}{Numeric. Response to Norse Feedback item Q39.}
#'     \item{\code{cog}}{Numeric. Score for the Cognitive problems/concentration problems scale.}
#'     \item{\code{control}}{Numeric. Score for the Control scale.}
#'     \item{\code{eating}}{Numeric. Score for the Eating problems scale.}
#'     \item{\code{genFunc}}{Numeric. Score for the General functioning scale.}
#'     \item{\code{hopeless}}{Numeric. Score for the Hopelessness/demoralization scale.}
#'     \item{\code{internal}}{Numeric. Score for the Internal avoidance scale.}
#'     \item{\code{irritable}}{Numeric. Score for the Irritability scale.}
#'     \item{\code{ready}}{Numeric. Score for the Readiness for recovery scale.}
#'     \item{\code{recovEnv}}{Numeric. Score for the Recovery environment scale.}
#'     \item{\code{sad}}{Numeric. Score for the Sadness/affect scale.}
#'     \item{\code{selfCrit}}{Numeric. Score for the Self-criticism scale.}
#'     \item{\code{avoidSit}}{Numeric. Score for the Situational avoidance scale.}
#'     \item{\code{avoidSoc}}{Numeric. Score for the Social avoidance scale.}
#'     \item{\code{socialSafety}}{Numeric. Score for the Social safety scale.}
#'     \item{\code{somAnx}}{Numeric. Score for the Somatic anxiety scale.}
#'     \item{\code{subRecov}}{Numeric. Score for the Substance recovery scale.}
#'     \item{\code{subUse}}{Numeric. Score for the Substance use scale.}
#'     \item{\code{suicide}}{Numeric. Score for the Suicide scale.}
#'     \item{\code{trauma}}{Numeric. Score for the Trauma reaction scale.}
#'     \item{\code{worry}}{Numeric. Score for the Worry scale.}
#'     \item{\code{alliance}}{Numeric. Mean score across the alliance items.}
#'     \item{\code{needs}}{Numeric. Mean score across the needs items.}
#' }
#'
#' @source See \code{data-raw/synthetic_data.R}.
"synthetic_data"

#' Research-friendly data from Helse Førde 2021.
#'
#' Anonymous research data from Helse Førde area (all clinics)
#' from May 2021, scored, with additional variables for external use.
#'
#' INCLUDES factor-scored subscale scores.
#'
#' Any values that were 99 are now NA, and these represent patient non-responses.
#'
#' THIS DATA IS NOT PROVIDED as part of the package and must be received separately
#' for confidentiality. Once received, the .rda file can be saved in the
#' global environment as \code{HF_research_data_2021 <- load("HF_research_data_2021.rda")}.
#' This process means that it is not really accessible as part of the package,
#' but must be stored locally.
#'
#' The codebook for the relevant items is provided here though.
#'
#' @format A data frame with 18620 observations of 250 variables.
#' \describe{
#'     \item{\code{anon_id}}{Numeric. Unique patient number within this data only, anonymized.}
#'     \item{\code{anon_tx_id}}{Numeric. Unique treatment number, anonymized.}
#'     \item{\code{date}}{Date. Date (and time) Norse Feedback was completed.}
#'     \item{\code{pt_first_date}}{Date. First date in the data for each respondent.}
#'     \item{\code{pt_wks_since_first}}{Numeric. Time difference in weeks from the patient's first date.}
#'     \item{\code{pt_order}}{Numeric. Observation number within patient across treatment types.}
#'     \item{\code{pt_total_obs}}{Numeric. Total number of observations for this patient.}
#'     \item{\code{pt_total_txs}}{Numeric. Total number of treatments for this patient.}
#'     \item{\code{pt_tx_order}}{Numeric. Treatment number within patient.}
#'     \item{\code{tx_first_date}}{Date. First date for this treatment.}
#'     \item{\code{tx_wks_since_first}}{Numeric. Time difference in weeks from the treatment's first date.}
#'     \item{\code{tx_order}}{Numeric. Observation number within treatment and respondent.}
#'     \item{\code{tx_total}}{Numeric. Total number of observations in this treatment.}
#'     \item{\code{treatment_name}}{Character. Treatment type or location as recorded in the source data.}
#'     \item{\code{tx_focus}}{Character. Treatment focus: Substance use, mental health, or unclear.}
#'     \item{\code{in_or_out}}{Character. Whether the treatment is inpatient, outpatient, or unclear.}
#'     \item{\code{is_MH}}{Logical. Whether the treatment is a mental-health service.}
#'     \item{\code{is_sub}}{Logical. Whether the treatment is a substance-use service.}
#'     \item{\code{is_inpt}}{Logical. Whether the treatment is an inpatient service.}
#'     \item{\code{is_outpt}}{Logical. Whether the treatment is an outpatient service.}
#'     \item{\code{pt_any_MH}}{Logical. Whether the patient has any mental-health service observations.}
#'     \item{\code{pt_any_sub}}{Logical. Whether the patient has any substance-use service observations.}
#'     \item{\code{pt_total_MH_tx}}{Numeric. Total number of mental-health service observations for this patient.}
#'     \item{\code{pt_total_sub_tx}}{Numeric. Total number of substance-use service observations for this patient.}
#'     \item{\code{sum_tx_cats}}{Numeric. Number of treatment categories assigned to the treatment name.}
#'     \item{\code{birthyear}}{Character. Year of birth for each respondent.}
#'     \item{\code{anon_assess_id}}{Numeric. Unique anonymized assessment-instance identifier.}
#'     \item{\code{cog}}{Numeric. Score for the Cognitive problems/concentration problems scale.}
#'     \item{\code{control}}{Numeric. Score for the Control scale.}
#'     \item{\code{eating}}{Numeric. Score for the Eating problems scale.}
#'     \item{\code{genFunc}}{Numeric. Score for the General functioning scale.}
#'     \item{\code{hopeless}}{Numeric. Score for the Hopelessness/demoralization scale.}
#'     \item{\code{internal}}{Numeric. Score for the Internal avoidance scale.}
#'     \item{\code{irritable}}{Numeric. Score for the Irritability scale.}
#'     \item{\code{ready}}{Numeric. Score for the Readiness for recovery scale.}
#'     \item{\code{recovEnv}}{Numeric. Score for the Recovery environment scale.}
#'     \item{\code{sad}}{Numeric. Score for the Sadness/affect scale.}
#'     \item{\code{selfCrit}}{Numeric. Score for the Self-criticism scale.}
#'     \item{\code{avoidSit}}{Numeric. Score for the Situational avoidance scale.}
#'     \item{\code{avoidSoc}}{Numeric. Score for the Social avoidance scale.}
#'     \item{\code{socialSafety}}{Numeric. Score for the Social safety scale.}
#'     \item{\code{somAnx}}{Numeric. Score for the Somatic anxiety scale.}
#'     \item{\code{subRecov}}{Numeric. Score for the Substance recovery scale.}
#'     \item{\code{subUse}}{Numeric. Score for the Substance use scale.}
#'     \item{\code{suicide}}{Numeric. Score for the Suicide scale.}
#'     \item{\code{trauma}}{Numeric. Score for the Trauma reaction scale.}
#'     \item{\code{worry}}{Numeric. Score for the Worry scale.}
#'     \item{\code{cog_first_pt}}{Numeric. First observed Cognitive problems/concentration problems score for this patient.}
#'     \item{\code{control_first_pt}}{Numeric. First observed Control score for this patient.}
#'     \item{\code{eating_first_pt}}{Numeric. First observed Eating problems score for this patient.}
#'     \item{\code{genFunc_first_pt}}{Numeric. First observed General functioning score for this patient.}
#'     \item{\code{hopeless_first_pt}}{Numeric. First observed Hopelessness/demoralization score for this patient.}
#'     \item{\code{internal_first_pt}}{Numeric. First observed Internal avoidance score for this patient.}
#'     \item{\code{irritable_first_pt}}{Numeric. First observed Irritability score for this patient.}
#'     \item{\code{ready_first_pt}}{Numeric. First observed Readiness for recovery score for this patient.}
#'     \item{\code{recovEnv_first_pt}}{Numeric. First observed Recovery environment score for this patient.}
#'     \item{\code{sad_first_pt}}{Numeric. First observed Sadness/affect score for this patient.}
#'     \item{\code{selfCrit_first_pt}}{Numeric. First observed Self-criticism score for this patient.}
#'     \item{\code{avoidSit_first_pt}}{Numeric. First observed Situational avoidance score for this patient.}
#'     \item{\code{avoidSoc_first_pt}}{Numeric. First observed Social avoidance score for this patient.}
#'     \item{\code{socialSafety_first_pt}}{Numeric. First observed Social safety score for this patient.}
#'     \item{\code{somAnx_first_pt}}{Numeric. First observed Somatic anxiety score for this patient.}
#'     \item{\code{subRecov_first_pt}}{Numeric. First observed Substance recovery score for this patient.}
#'     \item{\code{subUse_first_pt}}{Numeric. First observed Substance use score for this patient.}
#'     \item{\code{suicide_first_pt}}{Numeric. First observed Suicide score for this patient.}
#'     \item{\code{trauma_first_pt}}{Numeric. First observed Trauma reaction score for this patient.}
#'     \item{\code{worry_first_pt}}{Numeric. First observed Worry score for this patient.}
#'     \item{\code{assessment_version}}{Character. Norse Feedback back-end assessment version.}
#'     \item{\code{assessment_instance_title}}{Character. Assessment-instance title.}
#'     \item{\code{assessment_instance_start_date}}{Date. Assessment-instance start date.}
#'     \item{\code{assessment_instance_end_date}}{Date. Assessment-instance end date.}
#'     \item{\code{assessment_instance_created_date}}{Date. Assessment-instance creation date.}
#'     \item{\code{assessment_instance_last_modified_submitted}}{Date. Date the assessment instance was last modified or submitted.}
#'     \item{\code{assessment_instance_has_started}}{Logical. Whether the assessment instance has started.}
#'     \item{\code{assessment_instance_is_submitted}}{Logical. Whether the assessment instance was submitted.}
#'     \item{\code{assessment_instance_is_closed}}{Logical. Whether the assessment instance was closed.}
#'     \item{\code{assessment_instance_context_label}}{Character. Assessment context label, such as intake, weekly assessment, or termination.}
#'     \item{\code{assessment_instance_first_time_started_date}}{Date. First time the assessment instance was started.}
#'     \item{\code{assessment_instance_first_time_submitted_date}}{Date. First time the assessment instance was submitted.}
#'     \item{\code{treatment_type_id}}{Numeric. Treatment-type identifier.}
#'     \item{\code{treatment_type_name}}{Character. Treatment-type name.}
#'     \item{\code{respondent_gender}}{Character. Respondent gender.}
#'     \item{\code{respondent_account_enabled}}{Numeric. Source-system indicator that the respondent account is enabled.}
#'     \item{\code{respondent_test_account}}{Numeric. Source-system indicator that the respondent account is a test account.}
#'     \item{\code{respondent_last_login}}{Date. Respondent's last login date.}
#'     \item{\code{respondent_communication_disabled}}{Numeric. Source-system indicator that respondent communications are disabled.}
#'     \item{\code{Q42}}{Numeric. Response to Norse Feedback item Q42.}
#'     \item{\code{Q51}}{Numeric. Response to Norse Feedback item Q51.}
#'     \item{\code{Q46}}{Numeric. Response to Norse Feedback item Q46.}
#'     \item{\code{Q19}}{Numeric. Response to Norse Feedback item Q19.}
#'     \item{\code{Q59}}{Numeric. Response to Norse Feedback item Q59.}
#'     \item{\code{Q111}}{Numeric. Response to Norse Feedback item Q111.}
#'     \item{\code{Q37}}{Numeric. Response to Norse Feedback item Q37.}
#'     \item{\code{Q61}}{Numeric. Response to Norse Feedback item Q61.}
#'     \item{\code{Q39}}{Numeric. Response to Norse Feedback item Q39.}
#'     \item{\code{Q119}}{Numeric. Response to Norse Feedback item Q119.}
#'     \item{\code{Q64}}{Numeric. Response to Norse Feedback item Q64.}
#'     \item{\code{Q34}}{Numeric. Response to Norse Feedback item Q34.}
#'     \item{\code{Q38}}{Numeric. Response to Norse Feedback item Q38.}
#'     \item{\code{Q26}}{Numeric. Response to Norse Feedback item Q26.}
#'     \item{\code{Q43}}{Numeric. Response to Norse Feedback item Q43.}
#'     \item{\code{Q135}}{Numeric. Response to Norse Feedback item Q135.}
#'     \item{\code{Q80}}{Numeric. Response to Norse Feedback item Q80.}
#'     \item{\code{Q27}}{Numeric. Response to Norse Feedback item Q27.}
#'     \item{\code{Q146}}{Numeric. Response to Norse Feedback item Q146.}
#'     \item{\code{Q110}}{Numeric. Response to Norse Feedback item Q110.}
#'     \item{\code{Q103}}{Numeric. Response to Norse Feedback item Q103.}
#'     \item{\code{Q4}}{Numeric. Response to Norse Feedback item Q4.}
#'     \item{\code{Q126}}{Numeric. Response to Norse Feedback item Q126.}
#'     \item{\code{Q75}}{Numeric. Response to Norse Feedback item Q75.}
#'     \item{\code{Q107}}{Numeric. Response to Norse Feedback item Q107.}
#'     \item{\code{Q3}}{Numeric. Response to Norse Feedback item Q3.}
#'     \item{\code{Q113}}{Numeric. Response to Norse Feedback item Q113.}
#'     \item{\code{Q121}}{Numeric. Response to Norse Feedback item Q121.}
#'     \item{\code{Q53}}{Numeric. Response to Norse Feedback item Q53.}
#'     \item{\code{Q11}}{Numeric. Response to Norse Feedback item Q11.}
#'     \item{\code{Q12}}{Numeric. Response to Norse Feedback item Q12.}
#'     \item{\code{Q13}}{Numeric. Response to Norse Feedback item Q13.}
#'     \item{\code{Q14}}{Numeric. Response to Norse Feedback item Q14.}
#'     \item{\code{Q15}}{Numeric. Response to Norse Feedback item Q15.}
#'     \item{\code{Q84}}{Numeric. Response to Norse Feedback item Q84.}
#'     \item{\code{Q149}}{Numeric. Response to Norse Feedback item Q149.}
#'     \item{\code{Q150}}{Numeric. Response to Norse Feedback item Q150.}
#'     \item{\code{Q151}}{Numeric. Response to Norse Feedback item Q151.}
#'     \item{\code{Q78}}{Numeric. Response to Norse Feedback item Q78.}
#'     \item{\code{Q115}}{Numeric. Response to Norse Feedback item Q115.}
#'     \item{\code{Q120}}{Numeric. Response to Norse Feedback item Q120.}
#'     \item{\code{Q20}}{Numeric. Response to Norse Feedback item Q20.}
#'     \item{\code{Q136}}{Numeric. Response to Norse Feedback item Q136.}
#'     \item{\code{Q35}}{Numeric. Response to Norse Feedback item Q35.}
#'     \item{\code{Q122}}{Numeric. Response to Norse Feedback item Q122.}
#'     \item{\code{Q131}}{Numeric. Response to Norse Feedback item Q131.}
#'     \item{\code{Q24}}{Numeric. Response to Norse Feedback item Q24.}
#'     \item{\code{Q138}}{Numeric. Response to Norse Feedback item Q138.}
#'     \item{\code{Q40}}{Numeric. Response to Norse Feedback item Q40.}
#'     \item{\code{Q67}}{Numeric. Response to Norse Feedback item Q67.}
#'     \item{\code{Q124}}{Numeric. Response to Norse Feedback item Q124.}
#'     \item{\code{Q145}}{Numeric. Response to Norse Feedback item Q145.}
#'     \item{\code{Q139}}{Numeric. Response to Norse Feedback item Q139.}
#'     \item{\code{Q143}}{Numeric. Response to Norse Feedback item Q143.}
#'     \item{\code{Q105}}{Numeric. Response to Norse Feedback item Q105.}
#'     \item{\code{Q77}}{Numeric. Response to Norse Feedback item Q77.}
#'     \item{\code{Q104}}{Numeric. Response to Norse Feedback item Q104.}
#'     \item{\code{Q144}}{Numeric. Response to Norse Feedback item Q144.}
#'     \item{\code{Q114}}{Numeric. Response to Norse Feedback item Q114.}
#'     \item{\code{Q57}}{Numeric. Response to Norse Feedback item Q57.}
#'     \item{\code{Q142}}{Numeric. Response to Norse Feedback item Q142.}
#'     \item{\code{Q116}}{Numeric. Response to Norse Feedback item Q116.}
#'     \item{\code{Q123}}{Numeric. Response to Norse Feedback item Q123.}
#'     \item{\code{Q129}}{Numeric. Response to Norse Feedback item Q129.}
#'     \item{\code{Q17}}{Numeric. Response to Norse Feedback item Q17.}
#'     \item{\code{Q140}}{Numeric. Response to Norse Feedback item Q140.}
#'     \item{\code{Q88}}{Numeric. Response to Norse Feedback item Q88.}
#'     \item{\code{Q117}}{Numeric. Response to Norse Feedback item Q117.}
#'     \item{\code{Q127}}{Numeric. Response to Norse Feedback item Q127.}
#'     \item{\code{Q68}}{Numeric. Response to Norse Feedback item Q68.}
#'     \item{\code{Q65}}{Numeric. Response to Norse Feedback item Q65.}
#'     \item{\code{Q102}}{Numeric. Response to Norse Feedback item Q102.}
#'     \item{\code{Q130}}{Numeric. Response to Norse Feedback item Q130.}
#'     \item{\code{Q100}}{Numeric. Response to Norse Feedback item Q100.}
#'     \item{\code{Q18}}{Numeric. Response to Norse Feedback item Q18.}
#'     \item{\code{Q101}}{Numeric. Response to Norse Feedback item Q101.}
#'     \item{\code{Q147}}{Numeric. Response to Norse Feedback item Q147.}
#'     \item{\code{Q106}}{Numeric. Response to Norse Feedback item Q106.}
#'     \item{\code{Q141}}{Numeric. Response to Norse Feedback item Q141.}
#'     \item{\code{Q63}}{Numeric. Response to Norse Feedback item Q63.}
#'     \item{\code{Q109}}{Numeric. Response to Norse Feedback item Q109.}
#'     \item{\code{Q128}}{Numeric. Response to Norse Feedback item Q128.}
#'     \item{\code{Q132}}{Numeric. Response to Norse Feedback item Q132.}
#'     \item{\code{Q71}}{Numeric. Response to Norse Feedback item Q71.}
#'     \item{\code{Q72}}{Numeric. Response to Norse Feedback item Q72.}
#'     \item{\code{Q152}}{Numeric. Response to Norse Feedback item Q152.}
#'     \item{\code{Q153}}{Numeric. Response to Norse Feedback item Q153.}
#'     \item{\code{Q74}}{Numeric. Response to Norse Feedback item Q74.}
#'     \item{\code{Q154}}{Numeric. Response to Norse Feedback item Q154.}
#'     \item{\code{Q62}}{Numeric. Response to Norse Feedback item Q62.}
#'     \item{\code{Q50}}{Numeric. Response to Norse Feedback item Q50.}
#'     \item{\code{Q112}}{Numeric. Response to Norse Feedback item Q112.}
#'     \item{\code{Q108}}{Numeric. Response to Norse Feedback item Q108.}
#'     \item{\code{Q137}}{Numeric. Response to Norse Feedback item Q137.}
#'     \item{\code{Q118}}{Numeric. Response to Norse Feedback item Q118.}
#'     \item{\code{Q155}}{Numeric. Response to Norse Feedback item Q155.}
#'     \item{\code{Q134}}{Numeric. Response to Norse Feedback item Q134.}
#'     \item{\code{Q133}}{Numeric. Response to Norse Feedback item Q133.}
#'     \item{\code{Q10}}{Numeric. Response to Norse Feedback item Q10.}
#'     \item{\code{Q82}}{Numeric. Response to Norse Feedback item Q82.}
#'     \item{\code{Q83}}{Numeric. Response to Norse Feedback item Q83.}
#'     \item{\code{Q148}}{Numeric. Response to Norse Feedback item Q148.}
#'     \item{\code{alliance}}{Numeric. Mean score across the alliance items.}
#'     \item{\code{needs}}{Numeric. Mean score across the needs items.}
#'     \item{\code{cog_fscore}}{Numeric. Factor score for the Cognitive problems/concentration problems scale.}
#'     \item{\code{cog_fscore_se}}{Numeric. Standard error for the Cognitive problems/concentration problems factor score.}
#'     \item{\code{control_fscore}}{Numeric. Factor score for the Control scale.}
#'     \item{\code{control_fscore_se}}{Numeric. Standard error for the Control factor score.}
#'     \item{\code{eating_fscore}}{Numeric. Factor score for the Eating problems scale.}
#'     \item{\code{eating_fscore_se}}{Numeric. Standard error for the Eating problems factor score.}
#'     \item{\code{genFunc_fscore}}{Numeric. Factor score for the General functioning scale.}
#'     \item{\code{genFunc_fscore_se}}{Numeric. Standard error for the General functioning factor score.}
#'     \item{\code{hopeless_fscore}}{Numeric. Factor score for the Hopelessness/demoralization scale.}
#'     \item{\code{hopeless_fscore_se}}{Numeric. Standard error for the Hopelessness/demoralization factor score.}
#'     \item{\code{internal_fscore}}{Numeric. Factor score for the Internal avoidance scale.}
#'     \item{\code{internal_fscore_se}}{Numeric. Standard error for the Internal avoidance factor score.}
#'     \item{\code{irritable_fscore}}{Numeric. Factor score for the Irritability scale.}
#'     \item{\code{irritable_fscore_se}}{Numeric. Standard error for the Irritability factor score.}
#'     \item{\code{ready_fscore}}{Numeric. Factor score for the Readiness for recovery scale.}
#'     \item{\code{ready_fscore_se}}{Numeric. Standard error for the Readiness for recovery factor score.}
#'     \item{\code{recovEnv_fscore}}{Numeric. Factor score for the Recovery environment scale.}
#'     \item{\code{recovEnv_fscore_se}}{Numeric. Standard error for the Recovery environment factor score.}
#'     \item{\code{sad_fscore}}{Numeric. Factor score for the Sadness/affect scale.}
#'     \item{\code{sad_fscore_se}}{Numeric. Standard error for the Sadness/affect factor score.}
#'     \item{\code{selfCrit_fscore}}{Numeric. Factor score for the Self-criticism scale.}
#'     \item{\code{selfCrit_fscore_se}}{Numeric. Standard error for the Self-criticism factor score.}
#'     \item{\code{avoidSit_fscore}}{Numeric. Factor score for the Situational avoidance scale.}
#'     \item{\code{avoidSit_fscore_se}}{Numeric. Standard error for the Situational avoidance factor score.}
#'     \item{\code{avoidSoc_fscore}}{Numeric. Factor score for the Social avoidance scale.}
#'     \item{\code{avoidSoc_fscore_se}}{Numeric. Standard error for the Social avoidance factor score.}
#'     \item{\code{socialSafety_fscore}}{Numeric. Factor score for the Social safety scale.}
#'     \item{\code{socialSafety_fscore_se}}{Numeric. Standard error for the Social safety factor score.}
#'     \item{\code{somAnx_fscore}}{Numeric. Factor score for the Somatic anxiety scale.}
#'     \item{\code{somAnx_fscore_se}}{Numeric. Standard error for the Somatic anxiety factor score.}
#'     \item{\code{subRecov_fscore}}{Numeric. Factor score for the Substance recovery scale.}
#'     \item{\code{subRecov_fscore_se}}{Numeric. Standard error for the Substance recovery factor score.}
#'     \item{\code{subUse_fscore}}{Numeric. Factor score for the Substance use scale.}
#'     \item{\code{subUse_fscore_se}}{Numeric. Standard error for the Substance use factor score.}
#'     \item{\code{suicide_fscore}}{Numeric. Factor score for the Suicide scale.}
#'     \item{\code{suicide_fscore_se}}{Numeric. Standard error for the Suicide factor score.}
#'     \item{\code{trauma_fscore}}{Numeric. Factor score for the Trauma reaction scale.}
#'     \item{\code{trauma_fscore_se}}{Numeric. Standard error for the Trauma reaction factor score.}
#'     \item{\code{worry_fscore}}{Numeric. Factor score for the Worry scale.}
#'     \item{\code{worry_fscore_se}}{Numeric. Standard error for the Worry factor score.}
#'     \item{\code{cog_fs_first_pt}}{Numeric. First observed Cognitive problems/concentration problems factor score for this patient.}
#'     \item{\code{control_fs_first_pt}}{Numeric. First observed Control factor score for this patient.}
#'     \item{\code{eating_fs_first_pt}}{Numeric. First observed Eating problems factor score for this patient.}
#'     \item{\code{genFunc_fs_first_pt}}{Numeric. First observed General functioning factor score for this patient.}
#'     \item{\code{hopeless_fs_first_pt}}{Numeric. First observed Hopelessness/demoralization factor score for this patient.}
#'     \item{\code{internal_fs_first_pt}}{Numeric. First observed Internal avoidance factor score for this patient.}
#'     \item{\code{irritable_fs_first_pt}}{Numeric. First observed Irritability factor score for this patient.}
#'     \item{\code{ready_fs_first_pt}}{Numeric. First observed Readiness for recovery factor score for this patient.}
#'     \item{\code{recovEnv_fs_first_pt}}{Numeric. First observed Recovery environment factor score for this patient.}
#'     \item{\code{sad_fs_first_pt}}{Numeric. First observed Sadness/affect factor score for this patient.}
#'     \item{\code{selfCrit_fs_first_pt}}{Numeric. First observed Self-criticism factor score for this patient.}
#'     \item{\code{avoidSit_fs_first_pt}}{Numeric. First observed Situational avoidance factor score for this patient.}
#'     \item{\code{avoidSoc_fs_first_pt}}{Numeric. First observed Social avoidance factor score for this patient.}
#'     \item{\code{socialSafety_fs_first_pt}}{Numeric. First observed Social safety factor score for this patient.}
#'     \item{\code{somAnx_fs_first_pt}}{Numeric. First observed Somatic anxiety factor score for this patient.}
#'     \item{\code{subRecov_fs_first_pt}}{Numeric. First observed Substance recovery factor score for this patient.}
#'     \item{\code{subUse_fs_first_pt}}{Numeric. First observed Substance use factor score for this patient.}
#'     \item{\code{suicide_fs_first_pt}}{Numeric. First observed Suicide factor score for this patient.}
#'     \item{\code{trauma_fs_first_pt}}{Numeric. First observed Trauma reaction factor score for this patient.}
#'     \item{\code{worry_fs_first_pt}}{Numeric. First observed Worry factor score for this patient.}
#' }
#'
#' @source Processed from \code{NORSEpkg::hf.all.scored.2021.05.10}.
#' See \code{data-raw/HF_research_data_2021_fscores.R}.
"HF_research_data_2021_fscores"

#' Names of items
#'
#' @format An character vector of item names.
#'
#' @source See \code{data-raw/item_names_nf2.R}
#'
"item_names_nf2"


#' Item information, NF 3.1
#'
#' A summary of information about the NF items, updated 2022
#' @format A data frame with 96 rows and 16 variables:
#' \describe{
#'   \item{\code{assess_order}}{integer Order in assessment}
#'   \item{\code{item}}{character item name}
#'   \item{\code{reverse}}{character is the item reverse scored}
#'   \item{\code{trigger}}{logical is this a trigger item}
#'   \item{\code{simple_scale}}{character simple scale name}
#'   \item{\code{item_text_e}}{character Item text in English}
#'   \item{\code{scale_e}}{character Scale name in English}
#'   \item{\code{domain_e}}{character Domain in English}
#'   \item{\code{item_text_n}}{character COLUMN_DESCRIPTION}
#'   \item{\code{scale_n}}{character COLUMN_DESCRIPTION}
#'   \item{\code{domain_n}}{character COLUMN_DESCRIPTION}
#'   \item{\code{item_text_b}}{character COLUMN_DESCRIPTION}
#'   \item{\code{scale_b}}{character COLUMN_DESCRIPTION}
#'   \item{\code{domain_b}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Changes_on_items}}{character Were there item changes from 2.2 to 3?}
#'   \item{\code{Changes_on_dimensions}}{character Were there dimension changes from 2.2 to 3?}
#'}
#'
#' @details Not generally accessed directly. Use lookup functions like \code{\link{lookup_item}}.
#'
#' @source Based on 'NORSE Measure Master Document.xlsx', and more proximally 'data-raw/NF3.1_items.csv'.
"NF3.1_items"

#' Lookup table for Exported scores to useful scales
#'
#' A linking table for SCORE variables and human-readable scale names
#' @format A data frame with 47 rows and 2 variables:
#' \describe{
#'   \item{\code{ScoreName}}{character. Simplified name of the SCORE variable}
#'   \item{\code{ScaleName}}{character. Nicely formatted Scale name}
#' }
#' @details Could be incorporated into a scoring function.
#'
#' @source Based on exports of data provided by NF, and more proximally `data-raw/scoring_objects_raw.R`.
"scoreNames.nf3"

#' Internal scoring and lookup data
#'
#' Item-name vectors, scale-name mappings, and compatibility objects used
#' internally by NorseResearch. They are retained as package data for
#' backwards compatibility but are not generally needed directly.
#'
#' @name internal_data
#' @aliases alliance.names
#' @aliases alliance.names.nf3
#' @aliases anger.names.nf3
#' @aliases avoidSit.names
#' @aliases avoidSoc.names
#' @aliases cog.names
#' @aliases cog.names.nf3
#' @aliases control.names
#' @aliases eating.names
#' @aliases eating.names.nf3
#' @aliases genFunc.names
#' @aliases genFunc.names.nf3
#' @aliases hopeless.names
#' @aliases hopeless.names.nf3
#' @aliases impulsivity.names.nf3
#' @aliases intAvoid.names.nf3
#' @aliases internal.names
#' @aliases internal.names.nf3
#' @aliases intMem.names.nf3
#' @aliases irritable.names
#' @aliases list_objs
#' @aliases names.list
#' @aliases needs.names
#' @aliases needs.names.nf3
#' @aliases nicer_names_nf3
#' @aliases nicer.nf2.names
#' @aliases ona.names
#' @aliases ona.names.nf3
#' @aliases pain.names.nf3
#' @aliases physAnx.names.nf3
#' @aliases pref.names.nf3
#' @aliases ready.names
#' @aliases ready.names.nf3
#' @aliases recovEnv.names
#' @aliases sad.names
#' @aliases sad.names.nf3
#' @aliases scale_names
#' @aliases scale_names_nf3
#' @aliases scale_names_ou
#' @aliases SDH.names.nf3
#' @aliases selfComp.names.nf3
#' @aliases selfContempt.names.nf3
#' @aliases selfCrit.names
#' @aliases single.items.names
#' @aliases single.items.names.nf3
#' @aliases socAvoid.names.nf3
#' @aliases socSup.names.nf3
#' @aliases socialSafety.names
#' @aliases somAnx.names
#' @aliases subRecov.names
#' @aliases subUse.names
#' @aliases subUse.names.nf3
#' @aliases suicide.names
#' @aliases suicide.names.nf3
#' @aliases trauma.names
#' @aliases worry.names
#' @aliases worry.names.nf3
#' @docType data
#' @keywords internal
NULL
