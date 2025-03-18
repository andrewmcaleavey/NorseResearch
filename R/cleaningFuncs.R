
#' create a better-named patient_id variable in NF-like data
#'
#' @param .data Data frame
#' @param id Existing patient id variable name. Default is \code{`respondent id`},
#' a particularly terrible instance.
#' @param keep_old_vars Logical. Should the returned dataset include the
#' existing id variable? Defaults to \code{TRUE}. Implemented using \code{dplyr::mutate()}
#' \code{.keep} option.
#'
#' @return A data frame or tibble.
#' @export
#'
#' @examples
#' nicer_id_var(hf.scored.2019)
nicer_id_var <- function(.data,
                         id = `respondent id`,
                         keep_old_vars = TRUE) {

  # check if id is a column in .data
  if(!is.character(substitute(id))){
    # this is only for cases where the id variable is the oddly quoted version I hate.
    if(!as.character(quote(id)) %in% names(.data)) stop("id variable not found in .data.")
  } else {
    # this is for when the variable is provided as character.
    if(!id  %in% names(.data)) stop("id variable not found in .data.")
  }

  if(!keep_old_vars){
    return(.data %>%
             dplyr::mutate(patient_id = {{ id }},
                           .keep = "unused"))
  }

  else {
    .data %>%
      mutate(patient_id = {{ id }})
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
#' @description Note: This function does not sort the data by date.
#'
#' @examples
#' get_first_obs(hf.scored.2019)
#'
#' get_first_obs(HM_2020.02, id = "Pasientid")
#' # identical:
#' get_first_obs(HM_2020.02, id = Pasientid)
get_first_obs <- function(.data,
                          id = 'respondent id'){

  id.is.char <- FALSE
  try(id.is.char <- is.character(id),
      silent = TRUE)

  if(id.is.char){
    if(grepl(" ", id)){
      # this is only for cases where the id variable is the oddly quoted version I hate.
      tempout <- .data %>%
        group_by(id) %>%
        slice(1) %>%
        ungroup()
    }
    else {
      tempout <- .data %>%
        group_by_at(id) %>%
        slice(1) %>%
        ungroup()
    }
  }

  else if(!id.is.char){
    # this is for cases where the id variable is an unquoted string.
    tempout <- .data %>%
      group_by( {{ id }} ) %>%
      slice(1) %>%
      ungroup()
  }

  if(nrow(tempout) == 1){
    stop("\nOnly one row in output data. Likely because of a misspecified
         id variable.\n")
  }

  return(tempout)
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
#' values is non-missing. If both are non-missing, an error is thrown indicating the conflicting rows.
#' Otherwise, it updates the base variable with the companion value if the base variable is missing,
#' and then removes the companion variable from the data frame.
#'
#' @param df A data frame containing the Q-variables to be combined.
#' @param pattern_pre A regular expression pattern to identify the base Q-variables. Default is
#'   `"^Q\\d+$"`.
#' @param pattern_suff A suffix string to identify the companion variable. Default is `"_1"`.
#'
#' @return A data frame with the Q-variables combined. The base variables are updated with the
#'   combined values and the companion variables are dropped.
#'
#' @details The function first identifies all base Q-variables that match \code{pattern_pre} and
#'   then finds those for which a companion variable (with name equal to the base variable plus
#'   \code{pattern_suff}) exists. It uses \code{dplyr::mutate(across())} to process these variables:
#'   for each row, if the base variable is \code{NA} the function replaces it with the companion
#'   value. If both are non-\code{NA} for any row, the function stops with an error.
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
                           pattern_suff = "_1") {
  library(dplyr)

  # Identify base Q-variables matching pattern_pre.
  base_vars <- names(df)[grepl(pattern = pattern_pre, names(df))]
  # Select only those base_vars that have a companion variable.
  pairs <- base_vars[paste0(base_vars, pattern_suff) %in% names(df)]

  # Function to combine a base column with its companion column.
  combine_fun <- function(x) {
    col <- cur_column()
    comp_name <- paste0(col, pattern_suff)
    # Use pick() to access the full data mask and pull the companion column.
    comp <- pick(all_of(comp_name))[[1]]
    # Check that each row has at most one non-NA value in the pair.
    conflict <- ((!is.na(x)) + (!is.na(comp))) > 1
    if (any(conflict)) {
      stop(paste("Conflict in variable pair", col, "and", comp_name,
                 "in rows:", paste(which(conflict), collapse = ", ")))
    }
    # Combine: if base value is NA, use companion value.
    if_else(is.na(x), comp, x)
  }

  df_updated <- df %>%
    mutate(across(
      .cols = all_of(pairs),
      .fns = combine_fun
    ))

  # Remove the companion columns.
  companion_columns <- paste0(pairs, pattern_suff)
  df_updated <- df_updated %>% select(-all_of(companion_columns))

  return(df_updated)
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



