### DATA DOCUMENTATION


#' Item information, NF 2.1
#'
#' A summary of information about the NF items, updated September 2020
#' @format a data frame
#' \describe{
#'   \itemize{
#'     \item{\code{assess_order}} {Numeric. Order in assessment.}
#'     \item{\code{item}} {Character. Item name (e.g., "Q14").}
#'     \item{\code{reverse}} {Logical. Is item reverse scored?}
#'     \item{\code{trigger}} {Logical. Is the item used as an opening trigger?}
#'     \item{\code{item_text_e}} {Character. Text of the item in English.}
#'     \item{\code{simple_scale}} {Character. Scale name for programming.}
#'     \item{\code{scale_e}} {Character. Scale name for presentation.}
#'     \item{\code{domain_e}} {Character. Domain name in English.}
#'     \item{\code{item_text_n}} {Character. Text of the item in Nynorsk.}
#'     \item{\code{scale_n}} {Character. Scale name in Nynorsk.}
#'     \item{\code{domain_n}} {Character. Domain name in Nynorsk.}
#'     \item{\code{item_text_b}} {Character. Text of the item in Bokmål.}
#'     \item{\code{scale_b}} {Character. Scale name in Bokmål.}
#'     \item{\code{domain_b}} {Character. Domain name in Bokmål.}
#'   }
#' }
#' @usage View(nf2.1.item.descriptions)
#'
#' @details Not generally accessed directly. Use lookup functions like \code{\link{lookup_item}}.
#'
#' @source Based on 'NORSE Measure Master Document.xlsx', and more proximally 'NF2.1.items.xlsx'.
"nf2.1.item.descriptions"

#' NF v. 2.1 Logic
#'
#' A table to summarize opening and closing logic in NF 2.1
#' Added 7 September 2020.
#' @format A data frame.
#' \describe{
#'   \itemize{
#'     \item{\code{simple_scale}} {Character. Scale name for programming.}
#'     \item{\code{scale_e}} {Character. Scale name for presentation.}
#'     \item{\code{scale_n}} {Character. Scale name in Nynorsk.}
#'     \item{\code{trigger_item}} {Character. Item name (e.g., "Q145") of the trigger for this scale.}
#'     \item{\code{trigger_val}} {Numeric. Value of the opening threshold. Item response
#'     must be greater than this to open the scale.}
#'     \item{\code{closing_threshold}} {Numeric. Value of the closing threshold.
#'     Moving average of past 3 administrations must be below this value to close
#'     the scale.}
#'   }
#' }
#' @usage View(nf2.1.logic)
#'
#' @details Not generally accessed directly. Use lookup functions like \code{\link{lookup_item}}.
#' Used in scoring.
#'
#' @source Based on 'NORSE Measure Master Document.xlsx', and more proximally 'NF2.1.logic.xlsx'.
"nf2.1.logic"


#' Summary norm data for mental health specialty clinics, NF2, Helse Førde data, from 2019.
#' Only includes mean, SD, and N.
#'
#' @format a table.
#' \describe{
#'   \itemize{
#'     \item{\code{var}} {Variable in each row. Values: Mean, SD, and N}
#'     \item{\code{cog}} {Cognitive problems}
#'     \item{\code{ALLOTHERS}} {Other scales in NF2.1}
#'   }
#' }
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
#' @format A data frame with 18620 observations of 170 variables.
#' \describe{
#'   \itemize{
#'     \item{\code{anon_id}} {Numeric. Unique patient number within this data only, anonymized.}
#'     \item{\code{anon_tx_id}} {Numeric. Unique treatment number, anonymized.}
#'     \item{\code{date}} {Date. Date (and time) NF was completed.}
#'     \item{\code{pt_first_date}} {Date. First date in the data for each \code{respondent_id}.}
#'     \item{\code{pt_wks_since_first}} {Numeric. Time difference in weeks from \code{pt_first_date}.}
#'     \item{\code{pt_order}} {Numeric. Observation number within patient (all treatment types).}
#'     \item{\code{pt_total_obs}} {Numeric. Total number of observations (rows) of data for this patient.}
#'     \item{\code{pt_total_txs}} {Numeric. Total number of treatments (from \code{treatment_id}) for this patient.}
#'     \item{\code{pt_tx_order}} {Numeric. Treatment number within patient.}
#'     \item{\code{tx_first_date}} {Date. First date for this \code{treatment_id}.}
#'     \item{\code{tx_wks_since_first}} {Numeric. Time difference in weeks from \code{tx_first_date}.}
#'     \item{\code{tx_order}} {Numeric. Observation number within \code{treatment_id}, which is within \code{respondent_id}. }
#'     \item{\code{tx_total}} {Numeric. Total number of observations (rows) in this \code{treatment_id}.}
#'     \item{\code{treatment_name}} {Character. Data as input regarding treatment type/location.}
#'     \item{\code{tx_focus}} {Character. What was the focus of \code{treatment_name} for this observation.
#'     Values are "Sub", "MH", or "Unclear".}
#'     \item{\code{in_or_out}} {Character. Is the current \code{treatment_name} inpatient, outpatient, or unclear.}
#'     \item{\code{is_MH}} {Logical. Is this \code{treatment_name} a Mental Health service?}
#'     \item{\code{is_sub}} {Logical. Is this \code{treatment_name} a Substance Use service?}
#'     \item{\code{is_inpt}} {Logical. Is this \code{treatment_name} an inpatient service?}
#'     \item{\code{is_outpt}} {Logical. Is this \code{treatment_name} an outpatient service?}
#'     \item{\code{pt_any_MH}} {Logical. Did this patient contribute any mental health service observations (rows)?}
#'     \item{\code{pt_any_sub}} {Logical. Did this patient contribute any substance use service observations (rows)?}
#'     \item{\code{pt_total_MH_tx}} {Numeric. Total number MH observations (rows) for this patient.}
#'     \item{\code{pt_total_sub_tx}} {Numeric. Total number substance use observations (rows) for this patient.}
#'     \item{\code{sum_tx_cats}} {Numeric. Total number of categories this \code{treatment_name} fit.}
#'     \item{\code{birthyear}} {Character. Year of birth for each \code{respondent_id}.}
#'     \item{\code{assessment_instance_id}} {Numeric. Unique identifier for assessment instance (unique row number)}
#'     \item{\code{assessment_version}} {Character. Norse Feedback back-end version number (does not correspond to number used in other contexts).}
#'     \item{\code{assessment_instance_title}} {Character.}
#'     \item{\code{assessment_instance_start_date}} {Date.}
#'     \item{\code{assessment_instance_end_date}} {Date.}
#'     \item{\code{assessment_instance_created_date}} {Date.}
#'     \item{\code{assessment_instance_last_modified_submitted}} {Date.}
#'     \item{\code{assessment_instance_has_started}} {Logical.}
#'     \item{\code{assessment_instance_is_submitted}} {Logical.}
#'     \item{\code{assessment_instance_is_closed}} {Logical.}
#'     \item{\code{assessment_instance_context_label}} {Character. Options are "Inntak" (intake), "Vekesmåling" (weekly assessment), "Avslutning" (termination), and NA.}
#'     \item{\code{assessment_instance_first_time_started_date}} {Date.}
#'     \item{\code{assessment_instance_first_time_submitted_date}} {Date.}
#'     \item{\code{treatment_type_id}} {Numeric.}
#'     \item{\code{treatment_type_name}} {Character.}
#'     \item{\code{respondent_gender}} {Character. Options are "male" and "female".}
#'     \item{\code{respondent_account_enabled}} {Numeric.}
#'     \item{\code{respondent_test_account}} {Numeric.}
#'     \item{\code{respondent_last_login}} {Date.}
#'     \item{\code{respondent_communication_disabled}} {Numeric.}
#'   }
#' }
#'
#' @source Processed from NORSEpkg::hf.all.scored.2021.05.10.
#' See \code{data-raw/HF_research_data_2021.R}.
"HF_research_data_2021"


#' Synthetic Data (N = 500)
#'
#' This is not real data, it is essentially random numbers in the form of data
#' very similar to what Norse Feedback looks like. it is based on the function
#' random_norse_data().
#'
#' @format A data frame with 2883 observations of 132 variables.
#' \describe{
#'   \itemize{
#'     \item{\code{anon_id}} {Numeric. Unique patient number within this data only, anonymized.}
#'     \item{\code{date}} {Date. Date (and time) NF was completed.}
#'     \item{\code{pt_first_date}} {Date. First date in the data for each \code{respondent_id}.}
#'     \item{\code{pt_total_obs}} {Numeric. Total number of observations (rows) of data for this patient.}
#'     \item{\code{tx_focus}} {Character. What was the focus of \code{treatment_name} for this observation.
#'     Values are "Sub", "MH", or "Unclear".}
#'     \item{\code{in_or_out}} {Character. Is the current \code{treatment_name} inpatient, outpatient, or unclear.}
#'     \item{\code{birthyear}} {Character. Year of birth for each \code{respondent_id}.}
#'     \item{\code{gender}} {Character. Options are "male" and "female".}
#'     \item{code{OTHERS}} {Others including scale and item names.}
#'   }
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
#' @format A data frame with 18620 observations of 170 variables.
#' \describe{
#'   \itemize{
#'     \item{\code{anon_id}} {Numeric. Unique patient number within this data only, anonymized.}
#'     \item{\code{anon_tx_id}} {Numeric. Unique treatment number, anonymized.}
#'     \item{\code{date}} {Date. Date (and time) NF was completed.}
#'     \item{\code{pt_first_date}} {Date. First date in the data for each \code{respondent_id}.}
#'     \item{\code{pt_wks_since_first}} {Numeric. Time difference in weeks from \code{pt_first_date}.}
#'     \item{\code{pt_order}} {Numeric. Observation number within patient (all treatment types).}
#'     \item{\code{pt_total_obs}} {Numeric. Total number of observations (rows) of data for this patient.}
#'     \item{\code{pt_total_txs}} {Numeric. Total number of treatments (from \code{treatment_id}) for this patient.}
#'     \item{\code{pt_tx_order}} {Numeric. Treatment number within patient.}
#'     \item{\code{tx_first_date}} {Date. First date for this \code{treatment_id}.}
#'     \item{\code{tx_wks_since_first}} {Numeric. Time difference in weeks from \code{tx_first_date}.}
#'     \item{\code{tx_order}} {Numeric. Observation number within \code{treatment_id}, which is within \code{respondent_id}. }
#'     \item{\code{tx_total}} {Numeric. Total number of observations (rows) in this \code{treatment_id}.}
#'     \item{\code{treatment_name}} {Character. Data as input regarding treatment type/location.}
#'     \item{\code{tx_focus}} {Character. What was the focus of \code{treatment_name} for this observation.
#'     Values are "Sub", "MH", or "Unclear".}
#'     \item{\code{in_or_out}} {Character. Is the current \code{treatment_name} inpatient, outpatient, or unclear.}
#'     \item{\code{is_MH}} {Logical. Is this \code{treatment_name} a Mental Health service?}
#'     \item{\code{is_sub}} {Logical. Is this \code{treatment_name} a Substance Use service?}
#'     \item{\code{is_inpt}} {Logical. Is this \code{treatment_name} an inpatient service?}
#'     \item{\code{is_outpt}} {Logical. Is this \code{treatment_name} an outpatient service?}
#'     \item{\code{pt_any_MH}} {Logical. Did this patient contribute any mental health service observations (rows)?}
#'     \item{\code{pt_any_sub}} {Logical. Did this patient contribute any substance use service observations (rows)?}
#'     \item{\code{pt_total_MH_tx}} {Numeric. Total number MH observations (rows) for this patient.}
#'     \item{\code{pt_total_sub_tx}} {Numeric. Total number substance use observations (rows) for this patient.}
#'     \item{\code{sum_tx_cats}} {Numeric. Total number of categories this \code{treatment_name} fit.}
#'     \item{\code{birthyear}} {Character. Year of birth for each \code{respondent_id}.}
#'     \item{\code{assessment_instance_id}} {Numeric. Unique identifier for assessment instance (unique row number)}
#'     \item{\code{assessment_version}} {Character. Norse Feedback back-end version number (does not correspond to number used in other contexts).}
#'     \item{\code{assessment_instance_title}} {Character.}
#'     \item{\code{assessment_instance_start_date}} {Date.}
#'     \item{\code{assessment_instance_end_date}} {Date.}
#'     \item{\code{assessment_instance_created_date}} {Date.}
#'     \item{\code{assessment_instance_last_modified_submitted}} {Date.}
#'     \item{\code{assessment_instance_has_started}} {Logical.}
#'     \item{\code{assessment_instance_is_submitted}} {Logical.}
#'     \item{\code{assessment_instance_is_closed}} {Logical.}
#'     \item{\code{assessment_instance_context_label}} {Character. Options are "Inntak" (intake), "Vekesmåling" (weekly assessment), "Avslutning" (termination), and NA.}
#'     \item{\code{assessment_instance_first_time_started_date}} {Date.}
#'     \item{\code{assessment_instance_first_time_submitted_date}} {Date.}
#'     \item{\code{treatment_type_id}} {Numeric.}
#'     \item{\code{treatment_type_name}} {Character.}
#'     \item{\code{respondent_gender}} {Character. Options are "male" and "female".}
#'     \item{\code{respondent_account_enabled}} {Numeric.}
#'     \item{\code{respondent_test_account}} {Numeric.}
#'     \item{\code{respondent_last_login}} {Date.}
#'     \item{\code{respondent_communication_disabled}} {Numeric.}
#'     \item{\code{cog_fscore}} {Numeric. Cognitive Problems subscale, recoded with factor scoring. Same for other scale scores.}
#'     \item{\code{cog_fscore_se}} {Numeric. Cognitive Problems subscale factor score SE. Same for other scale scores.}
#'     \item{\code{cog_fs_first_pt}} {Numeric. First Cognitive Problems subscale score for this patient, scored as factor score. Same for other scale scores.}
#'     }
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
#' }
#' @usage View(NF3.1_items)
#'
#' @details Not generally accessed directly. Use lookup functions like \code{\link{lookup_item}}.
#'
#' @source Based on 'NORSE Measure Master Document.xlsx', and more proximally 'data-raw/NF3.1_items.csv'.
"NF3.1_items"
