#' NorseResearch: Utilities for Analyzing Norse Feedback Data
#'
#' @description
#' `NorseResearch` provides functions and documentation for working with data
#' exported from **Norse Feedback (NF)**, the Norwegian web-based outcome-measure
#' system used in mental-health and substance-use treatment. The package lets you
#' read NF exports, clean and reshape them, score the standard scales, and
#' inspect or plot the resulting items and scales.
#'
#' The package ships the code and item documentation, but **not** the patient
#' data itself, which is confidential. A small set of **synthetic** data is
#' bundled so the examples can be run without a real export.
#'
#' Most functions operate on a data frame whose item columns are named like
#' `Q42`, `Q102`, and so on. The bundled `synthetic_data` (an NF2-shaped data set
#' with a few pre-computed scales such as `sad` and `cog`) is the quickest way to
#' try the analysis and plotting functions; use [random_norse_data()] to generate
#' your own mock NF data.
#'
#' @section Key functions by task:
#' **Read and repair imports**
#'
#' - [read.csv_nf3()], [read.csv2_nf3()], [read_excel_nf3()], [read_password_protected_excel()], and [fix_failed_encoding()] read NF3 exports (comma- or semicolon-delimited, `.xlsx`, or password-protected) and repair common encoding problems.
#'
#' **Clean and reshape**
#'
#' - [clean_NF_names()], [collapse_versioned_columns()], [collapse_measures_wide()], [combine_suffix_variables()], [combine_q_vars()], [replace_98s_99s()], [nicer_id_var()], [get_first_obs()], and [get_first_nonmissing_obs()] standardize messy column names, widen repeated measures, and build clean respondent identifiers.
#'
#' **Versioning, lookup, and metadata**
#'
#' - [check_version_nf()] detects which NF version(s) are present, [check_nf_range()] validates item and score ranges, and [reverse_items()] exposes the authoritative reverse-item definitions. [lookup_item()] looks up item text by `Q`-number ([get_item_text()] is a deprecated alias). [get_nicer_name()] and [get_nf3_nicer_name()] turn short scale codes (e.g. `"sad"`) into readable names (e.g. `"Sad Affect"`). [lookup_trigger()] and [lookup_trigger_among()] identify trigger items.
#'
#' **Score scales**
#'
#' - [score_all()] and [score_all_nf3()] score NF2 and/or NF3 scales, including Overall Negative Affect (`ona`); [nf_score()] scores mixed-version data per row. [score_NORSE_trigger()] and [score_NORSE_overunder()] / [score_all_NORSE2_ou()] handle subthreshold trigger and over/under scoring. [score_normed_NF()] computes normed scores from published norms, and [rename_score_vars()] maps exported `SCORE_*` variables to consistent names.
#'
#' **Analyze and plot**
#'
#' - [scale_analysis2()] (use this; [scale_analysis()] is deprecated) gives reliability (omega, alpha) and IRT summaries for a scale. [scale_plot()], [item_plot()], and [trigger_plot()] show distributions, and [ggplot_icc_plot()] / [ggplot_information_plot()] draw item-characteristic and information-function plots for a `mirt`/`ltm` fit.
#'
#' **Generate mock NF data**
#'
#' - [random_norse_data()] builds a mock NF data set from [random_person_generator()] (person layer) and [random_tx_generator()] (treatment/observation layer).
#'
#' @section Bundled data:
#'
#' - `nf2.1.item.descriptions`, `nf2.1.logic` — NF 2.1 item metadata and opening/closing logic.
#' - `NF3.1_items` — NF 3.1 item metadata.
#' - `scoreNames.nf3` — mapping from exported `SCORE_*` names to readable scale names.
#' - `summary_norms_MH_out` — published norms (mean, SD, N) for NF2 mental-health scales.
#' - `item_names_nf2` — vector of NF2 item names.
#' - `synthetic_data` — small mock NF2 data set for the examples above.
#' - `HF_research_data_2021`, `HF_research_data_2021_fscores` — codebooks for the
#'   Helse Førde 2021 research data, which is **not** shipped with the package and
#'   must be obtained separately for confidentiality.
#'
#' @details
#' The introduction vignette walks through a full
#' pipeline from reading an export to scoring and plotting.
#'
#' A quick example:
#'
#' ```r
#' library(NorseResearch)
#' data(synthetic_data)
#'
#' check_version_nf(synthetic_data)              # "2"
#' get_nf3_nicer_name("sad")                    # "Sad Affect"
#' sa <- scale_analysis2("Sad affect", sad.names, synthetic_data)
#' print(sa)
#' plot(sa)
#' ```
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom lifecycle deprecated
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @importFrom dplyr across all_of arrange bind_cols coalesce cur_column everything filter group_by group_by_at if_else left_join mutate pick pull rename select slice starts_with transmute ungroup where
#' @importFrom ggplot2 aes aes_string element_blank facet_wrap geom_histogram geom_line geom_vline ggplot ggtitle labs scale_y_continuous stat_function theme theme_bw theme_minimal xlab ylab %+replace%
#' @importFrom gt cell_fill cell_text cells_body gt tab_header tab_style
#' @importFrom mirt fscores
#' @importFrom purrr map map2 set_names
#' @importFrom rlang .data ensym syms
#' @importFrom stringr str_detect str_extract
#' @importFrom tibble as_tibble rownames_to_column tibble
#' @importFrom tidyr gather
#' @importFrom stats aggregate coef cor dnorm rpois sd setNames
#' @importFrom utils read.csv read.csv2
## usethis namespace: end
NULL
