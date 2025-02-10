
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
