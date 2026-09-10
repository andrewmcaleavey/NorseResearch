
# Resolve a column name supplied either as a string (including a dynamically
# stored string) or as an unquoted column symbol.
resolve_id_name <- function(id) {
  expression <- if (rlang::is_quosure(id)) id else rlang::enquo(id)
  evaluation_failed <- FALSE
  evaluated <- tryCatch(
    rlang::eval_tidy(expression),
    error = function(e) {
      evaluation_failed <<- TRUE
      NULL
    }
  )
  if (!evaluation_failed) {
    if (!is.character(evaluated) || length(evaluated) != 1L || is.na(evaluated)) {
      if (rlang::is_symbol(rlang::get_expr(expression))) {
        return(rlang::as_name(rlang::get_expr(expression)))
      }
      stop("id must identify one column name.", call. = FALSE)
    }
    return(evaluated)
  }
  rlang::as_name(rlang::get_expr(expression))
}

#' create a better-named patient_id variable in NF-like data
#'
#' @param .data Data frame
#' @param id Existing patient id variable name. Default is "respondent id",
#' a particularly terrible instance.
#' @param keep_old_vars Logical. Should the returned dataset include the
#' existing id variable? Defaults to \code{TRUE}. Implemented using \code{dplyr::mutate()}
#' \code{.keep} option.
#'
#' @return A data frame or tibble.
#' @export
#'
#' @examples
#' data(synthetic_data, package = "NorseResearch")
#' nicer_id_var(synthetic_data, id = "anon_id")
nicer_id_var <- function(.data,
                         id = "respondent id",
                         keep_old_vars = TRUE) {
  id_name <- resolve_id_name(rlang::enquo(id))
  if (!id_name %in% names(.data)) stop("id variable not found in .data.")

  if(!keep_old_vars){
    return(.data %>%
             dplyr::mutate(patient_id = .data[[id_name]],
                           .keep = "unused"))
  }

  else {
    .data %>%
      mutate(patient_id = .data[[id_name]])
  }
}

#' Pick out only the first row per patient of a NF-type data frame.
#'
#' @param .data Data set on which to select rows
#' @param id Name of variable representing patient id. Default is `respondent id`.
#' Works if variable name is quoted or not.
#'
#' @return A tibble.
#' @export
#'
#' @description The first row is the first row in the input data for each ID;
#' the function does not sort by date or any other column. Arrange the data
#' before calling this function when a chronological first observation is
#' required. The ID may contain spaces and may be supplied quoted or unquoted.
#'
#' @examples
#' data(synthetic_data, package = "NorseResearch")
#' get_first_obs(synthetic_data, id = "anon_id")
get_first_obs <- function(.data,
                          id = 'respondent id'){
  if (!is.data.frame(.data)) {
    stop(".data must be a data frame or tibble.", call. = FALSE)
  }
  id_name <- resolve_id_name(rlang::enquo(id))
  if (length(id_name) != 1L || !id_name %in% names(.data)) {
    stop("id variable not found in .data.", call. = FALSE)
  }

  .data %>%
    dplyr::group_by(.data[[id_name]]) %>%
    dplyr::slice_head(n = 1L) %>%
    dplyr::ungroup()
}

#' Pick out the first non-missing observation per patient
#'
#' @param .data Data set on which to select rows.
#' @param id Name of the variable representing the patient ID. It may be
#'   supplied quoted or unquoted, and may contain spaces.
#' @param vars Character vector of columns used to identify a usable
#'   observation. Defaults to every column except `id`.
#' @param require Whether a usable row must contain a non-missing value in
#'   `"any"` or `"all"` of `vars`. The default is `"any"`.
#'
#' @return A tibble containing at most one row per ID. IDs with no qualifying
#'   row are omitted. Rows are considered in their existing input order; sort
#'   the data first if “first” should mean chronological order.
#' @export
#'
#' @examples
#' dat <- data.frame(
#'   `respondent id` = c("a", "a", "b"),
#'   score = c(NA, 2, NA),
#'   stringsAsFactors = FALSE,
#'   check.names = FALSE
#' )
#' get_first_nonmissing_obs(dat, "respondent id", vars = "score")
get_first_nonmissing_obs <- function(.data,
                                     id = "respondent id",
                                     vars = NULL,
                                     require = c("any", "all")) {
  if (!is.data.frame(.data)) {
    stop(".data must be a data frame or tibble.", call. = FALSE)
  }
  id_name <- resolve_id_name(rlang::enquo(id))
  if (length(id_name) != 1L || !id_name %in% names(.data)) {
    stop("id variable not found in .data.", call. = FALSE)
  }

  require <- match.arg(require)
  if (is.null(vars)) {
    vars <- setdiff(names(.data), id_name)
  } else if (!is.character(vars)) {
    stop("vars must be a character vector of column names.", call. = FALSE)
  }
  if (!length(vars)) {
    stop("vars must identify at least one column.", call. = FALSE)
  }
  missing_vars <- setdiff(vars, names(.data))
  if (length(missing_vars)) {
    stop("Unknown vars: ", paste(missing_vars, collapse = ", "), call. = FALSE)
  }

  usable <- if (require == "all") {
    rowSums(!is.na(.data[vars])) == length(vars)
  } else {
    rowSums(!is.na(.data[vars])) > 0L
  }

  .data[usable, , drop = FALSE] %>%
    dplyr::group_by(.data[[id_name]]) %>%
    dplyr::slice_head(n = 1L) %>%
    dplyr::ungroup()
}

#' Swap the short name of a scale for the nicer name of a scale.
#'
#' @param simplename A character matching a value in `scale_names`.
#' @param version A character string defining version to use. Defaults to
#' `2` for NF 2.x.
#'
#' @return A character from `nicer.nf2.names`. If no match found, returns `simplename`.
#' @export
#'
#' @examples
#' get_nicer_name("eating") # should return "Eating Problems"
#' get_nicer_name(c("eating", "subUse")) # should return a character vector
get_nicer_name <- function(simplename,
                           version = "2"){
  lifecycle::signal_stage("superseded",
                          what ="get_nicer_name()",
                          with = "get_nf3_nicer_name()")
  output <- rep(NA, length(simplename))
  if(version == "2"){
    output <- nicer.nf2.names[match(simplename, scale_names)]
  }
  if(any(is.na(output))){
    output[is.na(output)] <- simplename[is.na(output)]
    message("No nicer name identified, kept the same value")
  }

  return(output)
}

#' Swap the short name of a scale for the nicer name of a scale in NF 3.x
#'
#' @param simplename A character matching a value in `scale_names`.
#' @param version A character string defining version to use. Defaults to
#' `3` for NF 3.x.
#'
#' @return A character from `nicer.nf2.names`.
#' If no match found, returns `simplename`.
#' @export
#'
#' @examples
#' get_nf3_nicer_name("sad")
get_nf3_nicer_name <- function(simplename,
                               version = "3"){
  output <- rep(NA, length(simplename))
  if(version == "3"){
    output <- nicer_names_nf3[match(simplename, scale_names_nf3)]
  }
  if(any(is.na(output))){
    output[is.na(output)] <- simplename[is.na(output)]
    message("No nicer name identified, kept the same value")
  }

  return(output)
}

#' Combine Paired Q Variables
#'
#' This function combines paired Q-variables in a data frame. A paired Q-variable consists of a
#' base variable and a companion variable whose name is formed by appending a suffix (default: "_1")
#' to the base variable name. For each row, the function first checks that at most one of the two
#' values is non-missing. By default, if both are non-missing, an error is
#' thrown indicating the conflicting rows. The explicit `conflict =
#' "highest_suffix"` mode is available for legacy exports that need a
#' deterministic compatibility rule. Otherwise, it updates the base variable
#' with the companion value if the base variable is missing and removes the
#' companion variable from the data frame.
#'
#' @param df A data frame containing the Q-variables to be combined.
#' @param pattern_pre A regular expression pattern to identify the base Q-variables. Default is
#'   `"^Q\\d+$"`.
#' @param pattern_suff A suffix string to identify the companion variable. Default is `"_1"`.
#' @param conflict Conflict policy for rows in which both paired values are
#'   present. The default, `"error"`, keeps this function a strict paired-column
#'   validator. `"highest_suffix"` is an explicit compatibility mode that
#'   delegates to [collapse_versioned_columns()].
#'
#' @return A data frame with the Q-variables combined. The base variables are updated with the
#'   combined values and the companion variables are dropped.
#'
#' @details The function first identifies all base Q-variables that match \code{pattern_pre} and
#'   then finds those for which a companion variable (with name equal to the base variable plus
#'   \code{pattern_suff}) exists. It delegates the merge to
#'   [collapse_versioned_columns()]. In the default strict mode, if both are
#'   non-\code{NA} for any row, the function stops with an error.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(Q140 = c(NA, 2, 3),
#'                  Q140_1 = c(1, NA, NA),
#'                  Q150 = c(4, NA, 6),
#'                  Q150_1 = c(NA, 5, NA),
#'                  stringsAsFactors = FALSE)
#' df_combined <- combine_q_vars(df)
#' }
#'
#' @export
combine_q_vars <- function(df,
                           pattern_pre = "^Q\\d+$",
                           pattern_suff = "_1",
                           conflict = c("error", "highest_suffix")) {
  if (!is.data.frame(df)) {
    stop("df must be a data frame or tibble.", call. = FALSE)
  }
  if (!is.character(pattern_pre) || length(pattern_pre) != 1L ||
      is.na(pattern_pre) || !is.character(pattern_suff) ||
      length(pattern_suff) != 1L || is.na(pattern_suff) ||
      !nzchar(pattern_suff)) {
    stop("pattern_pre and pattern_suff must be single character strings.",
         call. = FALSE)
  }
  conflict <- match.arg(conflict)
  # Identify base Q-variables matching pattern_pre.
  base_vars <- names(df)[grepl(pattern = pattern_pre, names(df))]
  # Select only those base_vars that have a companion variable.
  pairs <- base_vars[paste0(base_vars, pattern_suff) %in% names(df)]

  suffix_pattern <- paste0(stringr::str_escape(pattern_suff), "$")
  collapse_versioned_columns(
    df,
    suffix_pattern = suffix_pattern,
    conflict = conflict,
    coerce = "common",
    base_names = pairs
  )
}

#' Rename SCORE Variables Using a Mapping Table
#'
#' This function renames SCORE variables in a data frame by replacing their names with
#' human-readable scale names provided in a mapping table. The mapping table must contain
#' two columns: \code{ScoreName} (the original variable names, e.g., "SCORE_G10") and
#' \code{ScaleName} (the corresponding human-readable names, e.g., "Physical Anxiety").
#' An optional prefix (default \code{"score_"}) is prepended to the cleaned scale name.
#' Cleaning involves replacing spaces with underscores and removing non-alphanumeric characters,
#' ensuring that the new variable names are syntactically valid.
#'
#' @param df A data frame containing the SCORE variables to be renamed.
#' @param prefix A character string to prepend to the cleaned scale name. Defaults to \code{"score_"}.
#' @param mapping A data frame with two columns: \code{ScoreName} (the original variable names)
#'   and \code{ScaleName} (the desired human-readable names). Defaults to \code{scoreNames.nf3}.
#'
#' @return A data frame with SCORE variables renamed to the new names.
#'
#' @details For each SCORE variable found in \code{df} that matches a \code{ScoreName} in \code{mapping},
#'   the function builds a new variable name by concatenating \code{prefix} with a cleaned version
#'   of the corresponding \code{ScaleName}. Cleaning is done by replacing spaces with underscores and
#'   removing any characters other than letters, digits, and underscores. If duplicate names result,
#'   \code{make.unique} is used to ensure uniqueness.
#'
#' @examples
#' \dontrun{
#' # Assume scoreNames.nf3 is available and looks like this:
#' scoreNames.nf3 <- data.frame(
#'   ScoreName = c("SCORE_G10", "SCORE_G11", "SCORE_G12"),
#'   ScaleName = c("Physical Anxiety", "Self-Compassion", "Emotional Resilience"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Example data frame:
#' df <- data.frame(
#'   SCORE_G10 = rnorm(10),
#'   SCORE_G11 = rnorm(10),
#'   SCORE_G12 = rnorm(10),
#'   OtherVar  = letters[1:10],
#'   stringsAsFactors = FALSE
#' )
#'
#' # Rename the SCORE variables.
#' df_new <- rename_score_vars(df)
#' # New names will be like "score_Physical_Anxiety", "score_SelfCompassion", and "score_Emotional_Resilience"
#' }
#'
#' @export
rename_score_vars <- function(df,
                              prefix = "score_",
                              mapping = scoreNames.nf3) {
  # Check that mapping has the required columns
  if (!all(c("ScoreName", "ScaleName") %in% names(mapping))) {
    stop("Mapping must have columns 'ScoreName' and 'ScaleName'.")
  }

  # Identify which columns in df are SCORE variables found in the mapping.
  score_vars <- intersect(names(df), mapping$ScoreName)

  # If no SCORE variables found, return df unmodified.
  if (length(score_vars) == 0) return(df)

  # Create new variable names from the mapping.
  new_names <- sapply(score_vars, function(old_name) {
    # Get the human-readable scale name for this variable.
    scale <- mapping$ScaleName[mapping$ScoreName == old_name][1]
    # Replace spaces with underscores and remove non-alphanumeric characters (except underscores).
    clean_scale <- gsub("[^A-Za-z0-9_]", "", gsub(" ", "_", scale))
    paste0(prefix, clean_scale)
  }, USE.NAMES = FALSE)

  # Make names unique if necessary.
  new_names <- make.unique(new_names)

  # Create a named vector for dplyr::rename(), with names as the new names and values as the old names.
  rename_vec <- setNames(score_vars, new_names)

  # Rename the columns in df.
  df <- dplyr::rename(df, !!!rename_vec)

  return(df)
}
