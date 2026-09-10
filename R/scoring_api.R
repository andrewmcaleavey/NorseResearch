# Shared scoring metadata and validation helpers.

# Normalize the version labels used by the scoring functions. Public scoring
# functions historically accepted both the short labels ("2", "3") and labels
# such as "NF2" or "3.1"; internally we use the stable short labels.
normalize_nf_versions <- function(version, arg = "version") {
  if (!is.character(version) && !is.numeric(version)) {
    stop(arg, " must contain NF version labels.", call. = FALSE)
  }
  if (!length(version)) {
    stop(arg, " must contain at least one NF version label.", call. = FALSE)
  }
  version <- toupper(trimws(as.character(version)))
  version <- gsub("\\s+", "", version)
  canonical <- rep(NA_character_, length(version))
  canonical[grepl("^(NF)?2(?:\\.\\d+)?$", version)] <- "2"
  canonical[grepl("^(NF)?3(?:\\.\\d+)?$", version)] <- "3"
  if (anyNA(canonical)) {
    bad <- unique(version[is.na(canonical)])
    stop("Unsupported NF version(s): ", paste(bad, collapse = ", "),
         ". Use '2'/'NF2' and/or '3'/'NF3'.", call. = FALSE)
  }
  unique(canonical)
}

normalize_nf_version_values <- function(x, versions = c("2", "3")) {
  versions <- normalize_nf_versions(versions, arg = "versions")
  raw <- toupper(trimws(as.character(x)))
  raw <- gsub("\\s+", "", raw)
  out <- rep(NA_character_, length(raw))
  for (version in versions) {
    out[grepl(paste0("^(NF)?", version, "(?:\\.\\d+)?$"), raw)] <- version
  }
  out
}

#' Return the authoritative reverse-scored item names
#'
#' @param version NF version label. `"2"`, `"2.1"`, `"NF2"`, `"3"`,
#'   `"3.1"`, and `"NF3"` are accepted and normalized to NF2 or NF3.
#'
#' @return A character vector of item names marked as reverse scored in the
#'   bundled version-specific item metadata.
#' @export
#'
#' @examples
#' reverse_items("NF2")
reverse_items <- function(version = "2") {
  version <- normalize_nf_versions(version)
  if (length(version) != 1L) {
    stop("version must identify exactly one NF version.", call. = FALSE)
  }
  table <- if (version == "2") nf2.1.item.descriptions else NF3.1_items
  reverse <- table$reverse
  is_reverse <- if (is.logical(reverse)) {
    !is.na(reverse) & reverse
  } else {
    toupper(trimws(as.character(reverse))) %in% c("TRUE", "T", "R", "YES", "1")
  }
  unique(table$item[is_reverse & !is.na(table$item)])
}

#' Check that NF item or score values are within their response range
#'
#' @param dat A data frame.
#' @param vars Character vector of columns to check. If `NULL`, columns with
#'   names matching `Q#` are checked.
#' @param lower Inclusive lower bound. Defaults to `1`.
#' @param upper Inclusive upper bound. Defaults to `7`.
#' @param action What to do when a value is invalid: `"error"` (default),
#'   `"warn"`, or `"logical"` (return `FALSE` without a warning).
#'
#' @return Invisibly `TRUE` when all checked values are valid. With
#' `action = "logical"`, returns `TRUE` or `FALSE` visibly.
#' @export
#'
#' @examples
#' check_nf_range(data.frame(Q1 = c(1, 7)))
check_nf_range <- function(dat,
                           vars = NULL,
                           lower = 1,
                           upper = 7,
                           action = c("error", "warn", "logical")) {
  if (!is.data.frame(dat)) {
    stop("dat must be a data frame or tibble.", call. = FALSE)
  }
  if (!is.numeric(lower) || length(lower) != 1L ||
      !is.numeric(upper) || length(upper) != 1L ||
      is.na(lower) || is.na(upper) || lower > upper) {
    stop("lower and upper must be ordered numeric bounds.", call. = FALSE)
  }
  action <- match.arg(action)

  if (is.null(vars)) {
    vars <- names(dat)[grepl("^Q[0-9]+(?:[._][0-9]+)?$", names(dat), perl = TRUE)]
  } else if (!is.character(vars)) {
    stop("vars must be a character vector of column names.", call. = FALSE)
  }
  missing_vars <- setdiff(vars, names(dat))
  if (length(missing_vars)) {
    stop("Unknown vars: ", paste(missing_vars, collapse = ", "), call. = FALSE)
  }
  if (!length(vars)) {
    return(if (action == "logical") TRUE else invisible(TRUE))
  }

  invalid <- vapply(vars, function(var) {
    x <- dat[[var]]
    text <- if (is.factor(x)) as.character(x) else x
    numeric_x <- suppressWarnings(as.numeric(text))
    nonmissing <- !is.na(text)
    any(nonmissing & (is.na(numeric_x) | numeric_x < lower | numeric_x > upper))
  }, logical(1))

  if (any(invalid)) {
    bad_vars <- vars[invalid]
    message_text <- paste0(
      "Some NF values outside scoring range [", lower, ", ", upper,
      "] in: ", paste(bad_vars, collapse = ", "),
      ". Check that missing values are coded as NA and item responses are valid."
    )
    if (action == "error") stop(message_text, call. = FALSE)
    if (action == "warn") warning(message_text, call. = FALSE)
    if (action == "logical") return(FALSE)
  }

  if (action == "logical") TRUE else invisible(TRUE)
}
