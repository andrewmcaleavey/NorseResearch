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
#' @details This reports historical wording flags. Scoring policy overrides
#' these flags for Alliance, Therapy Preferences/Needs, QOL, and Norse items,
#' which are never reversed by [prepare_nf_items()] or the scoring wrappers.
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

# Explicit exceptions to problem-oriented scoring. These response scales keep
# their exported direction, even where historical item metadata says reverse.
nf_scoring_exceptions <- function(version) {
  version <- normalize_nf_versions(version)
  unique(c("Q226", "Q148", "Q148.2",
    if ("2" %in% version) c(alliance.names, needs.names),
    if ("3" %in% version) c(alliance.names.nf3, pref.names.nf3)))
}

# Ordinary response values differ for a small number of items. Keep this policy
# next to the other shared scoring metadata so audits and scorers validate the
# same values.
nf_item_response_values <- function(item) {
  if (identical(item, "Q226")) 0:10 else 1:7
}

#' Prepare NF item responses for scoring
#'
#' Standardize problem and resource items to higher = more problems, handling
#' special response codes separately from ordinary responses. Alliance, Therapy
#' Preferences, QOL (Q226), and Norse (Q148, including Q148.2) retain their
#' exported direction and treat both -98 and -99 as missing.
#'
#' @param dat A data frame with short item names and numeric responses. Known
#'   item columns must contain their ordinary response values, -98, -99, or
#'   `NA` on scored rows. Most NF items use 1--7; QOL item Q226 uses 0--10.
#' @param input_coding Required character value: `"higher_is_worse"` for already
#'   standardized exports or `"agreement"` for original agreement responses.
#'   May also be a vector of length `nrow(dat)` for reviewed batch/row conventions.
#'   Coding is never guessed from correlations.
#' @param version NF version, or a vector of versions of length `nrow(dat)`.
#'   Defaults to `"3"`. Accepts the aliases used by [score_all()]. Missing
#'   versions mark unscored rows, whose known items become missing in this
#'   prepared copy. Nonmissing unsupported versions are errors.
#' @param item_coding Optional named character vector overriding `input_coding`
#'   for specific known item columns, using the same two coding labels. Each
#'   override applies to all rows where that item is active. Exceptions retain
#'   their direction regardless of the supplied coding label. For more complex
#'   item-by-batch exceptions, prepare separate batches.
#'
#' @details
#' Ordinary 1--7 positive agreement responses become `8 - x`; other ordinary
#' responses stay unchanged. Q226 is an unreversed 0--10 item. On
#' problem-oriented items, -98 becomes 1 and -99 becomes `NA`. On the named
#' exceptions both codes become `NA`. A recoded -98 is never subsequently
#' reversed. Only metadata-defined items and explicit exception items are
#' prepared; other columns are untouched. Items belonging only to inactive
#' versions become missing on those rows.
#'
#' This function returns modified item columns. In contrast, [score_all()] and
#' the version-specific scoring wrappers prepare an internal copy and preserve
#' the caller's original item columns in their returned data. Do not prepare
#' agreement responses twice: pass prepared data to a scorer with
#' `input_coding = "higher_is_worse"`. Neither an audit verdict nor an item flag
#' certifies the coding of unassessed items. Review the convention first.
#'
#' @return A data frame with standardized item columns. Non-item columns are
#'   unchanged. The result contains no special codes in active known items.
#' @seealso [audit_nf_reverse()], [score_all()], [reverse_items()]
#' @examples
#' dat <- data.frame(Q223 = c(7, -98, -99), Q226 = c(7, -98, -99))
#' prepare_nf_items(dat, input_coding = "agreement", version = "3")
#' @export
prepare_nf_items <- function(dat, input_coding, version = "3", item_coding = NULL) {
  if (!is.data.frame(dat) || anyDuplicated(names(dat)))
    stop("dat must be a data frame with unique column names.", call. = FALSE)
  labels <- c("higher_is_worse", "agreement")
  if (missing(input_coding) || !is.character(input_coding) || anyNA(input_coding) ||
      !length(input_coding) || !length(input_coding) %in% c(1L, nrow(dat)) ||
      any(!input_coding %in% labels))
    stop("input_coding must be 'higher_is_worse' or 'agreement', once or per row.", call. = FALSE)
  coding <- rep(input_coding, length.out = nrow(dat))
  if ((!is.character(version) && !is.numeric(version)) || !length(version) ||
      !length(version) %in% c(1L, nrow(dat)))
    stop("version must be supplied once or per row.", call. = FALSE)
  v <- rep(as.character(version), length.out = nrow(dat))
  if (any(!is.na(v))) {
    normalize_nf_versions(v[!is.na(v)])
    v <- normalize_nf_version_values(v)
  }
  tables <- list("2" = nf2.1.item.descriptions, "3" = NF3.1_items)
  known <- unique(c(tables[[1]]$item, tables[[2]]$item, nf_scoring_exceptions(c("2", "3"))))
  if (!is.null(item_coding) && (!is.character(item_coding) ||
      is.null(names(item_coding)) || anyNA(item_coding) || anyNA(names(item_coding)) ||
      anyDuplicated(names(item_coding)) || any(!names(item_coding) %in% intersect(known, names(dat))) ||
      any(!item_coding %in% labels)))
    stop("item_coding must map known exported item names to valid coding labels.", call. = FALSE)
  output <- dat
  for (item in intersect(known, names(dat))) {
    output[[item]] <- rep(NA_real_, nrow(dat))
    for (ver in c("2", "3")) {
      exceptions <- nf_scoring_exceptions(ver)
      if (!item %in% c(tables[[ver]]$item, exceptions)) next
      rows <- which(!is.na(v) & v == ver)
      if (!length(rows)) next
      x <- dat[[item]][rows]
      ordinary_values <- nf_item_response_values(item)
      if (!is.numeric(x) || !is.null(dim(dat[[item]])) ||
          any(!is.na(x) & !x %in% c(ordinary_values, -98, -99))) {
        expected <- if (identical(item, "Q226")) "0:10" else "1:7"
        stop("Invalid item responses in ", item, "; expected numeric ",
             expected, ", -98, -99, or NA.", call. = FALSE)
      }
      missing_code <- is.na(x) | x == -99
      no_problem <- !is.na(x) & x == -98
      ordinary <- x %in% ordinary_values
      direction <- coding[rows]
      if (item %in% names(item_coding)) direction[] <- item_coding[[item]]
      if (!item %in% exceptions && item %in% reverse_items(ver)) {
        flip <- ordinary & direction == "agreement"
        x[flip] <- 8 - x[flip]
      }
      x[missing_code] <- NA_real_
      x[no_problem] <- if (item %in% exceptions) NA_real_ else 1
      output[[item]][rows] <- x
    }
  }
  output
}

# Restore source columns after scoring a prepared working copy.
restore_nf_source <- function(scored, original, score_vars) {
  for (col in setdiff(names(original), score_vars)) scored[[col]] <- original[[col]]
  scored
}
