#' check if NORSE scores are reversed
#'
#' @param .data The data to clean.
#' @param verbose Logical. Default is FALSE. If TRUE, will
#' return the list of correlations tested with their values.
#' @param version Which version of NF is being used. Stable labels are `"NF2"`
#' and `"NF3"`; compatible short and minor-version forms such as `"2"`,
#' `"2.1"`, `"3"`, and `"3.1"` are also accepted.
#'
#' @return Logical. Is data likely already reversed? `TRUE` means the checked
#' correlations are non-negative. If \code{verbose} is `TRUE`, returns a named
#' list with the decision and each correlation. A data set with no complete
#' observations for any check returns `NA`.
#' @export
#'
#' @examples
#' data(synthetic_data, package = "NorseResearch")
#' check_rev(synthetic_data)
#' check_rev(synthetic_data, verbose = TRUE)
#'
check_rev <- function(.data, verbose = FALSE, version = "NF2") {
  if (!is.data.frame(.data)) {
    stop(".data must be a data frame or tibble.", call. = FALSE)
  }
  version <- tryCatch(
    normalize_nf_versions(version),
    error = function(e) {
      stop("Incorrect version provided. Only use either NF2 or NF3.", call. = FALSE)
    }
  )
  if (length(version) != 1L) {
    stop("Incorrect version provided. Supply exactly one of NF2 or NF3.", call. = FALSE)
  }

  q_vars <- names(.data)[grepl("^Q[0-9]+(?:[._][0-9]+)?$", names(.data), perl = TRUE)]
  q_vars <- setdiff(q_vars, c("Q226", "Q71", "Q72", "Q152", "Q153", "Q74"))
  check_nf_range(.data, vars = q_vars, action = "error")

  pair_names <- if (version == "2") {
    c("rQ15.Q115" = "Q15|Q115",
      "rQ27.Q141" = "Q27|Q141",
      "rQ140.Q141" = "Q140|Q141",
      "rQ10.Q123" = "Q10|Q123",
      "rQ67.Q126" = "Q67|Q126")
  } else {
    c("rQ202.Q215" = "Q202|Q215",
      "rQ140.Q207" = "Q140|Q207",
      "rQ211.Q215" = "Q211|Q215",
      "rQ217.Q207" = "Q217|Q207",
      "rQ223.Q205" = "Q223|Q205",
      "rQ220.Q207" = "Q220|Q207")
  }
  pair_vars <- strsplit(unname(pair_names), "\\|", fixed = FALSE)
  missing <- unique(unlist(lapply(pair_vars, setdiff, names(.data))))
  if (length(missing)) {
    stop("Missing item columns required for reversal check: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }

  correlations <- vapply(pair_vars, function(pair) {
    x <- suppressWarnings(as.numeric(as.character(.data[[pair[[1L]]]])))
    y <- suppressWarnings(as.numeric(as.character(.data[[pair[[2L]]]])))
    suppressWarnings(stats::cor(x, y, use = "complete.obs"))
  }, numeric(1))
  names(correlations) <- names(pair_names)

  reversed <- if (all(is.na(correlations))) {
    NA
  } else {
    !any(correlations < 0, na.rm = TRUE)
  }
  if (!verbose) return(reversed)
  c(list(reversed = reversed), as.list(correlations))
}

# need a function to find and properly treat any -98 or -99 values
# in NF 3, the default should be any -98 (not relevant) is 1
# and any -99 is NA.

#' Replace Specific Values in a Data Frame
#'
#' This function takes a data frame and replaces all instances of the value
#' `-98` with `1`, and all instances of `-99` with `NA`. It works across both
#' numeric and character variables.
#'
#' @param dat A data frame whose values will be modified. The function can
#' handle columns containing either numeric or character types, but the
#' replacement logic applies only to numeric-like columns (i.e., integer
#' or double).
#'
#' @return A data frame of the same structure as `dat`, with specified
#' replacements made.
#'
#' @export
#'
#' @examples
#' df <- data.frame(a = c(1, -98, -99), b = c("text", "more text", "-98"),
#' stringsAsFactors = FALSE)
#' new_df <- replace_98s_99s(df)
#' print(new_df)
#'
replace_98s_99s <- function(dat){
  dat %>%
    mutate(across(everything(),
                  ~ ifelse(. == -98, 1,
                           ifelse(. == -99, NA_real_, .))))
}
# replace_98s_99s(calData)


#' Conditionally drop variables without an error
#'
#' @param df data
#' @param var_name a character name of a variable
#'
#' @returns data
#'
#' @export
#'
#' @examples
#' df <- data.frame(A = 1:5, B = letters[1:5], C = rnorm(5))
#' drop_variable(df, "C")
#' drop_variable(df, "D")
drop_variable <- function(df, var_name) {
  if (exists(var_name, where = as.environment(df))) {
    df <- df %>% dplyr::select(-all_of(var_name))
  }
  return(df)
}
