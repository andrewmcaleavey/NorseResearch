#' Clean variable names of a Norse Feedback data frame
#'
#' @param .data The data to clean
#' @param unique Logical. Should unique variable names be permitted?
#'
#' @return A data frame with clean names
#' @export
#'
#' @examples
#'  clean_NF_names(
#'    c(
#'    "  a", "a  ",
#'    "a %", "a", "$a", "$$$a", "GDP ($)", "GDP (us$)",
#'    "a (#)", "a & b", "#", "$",
#'    "a_cnt",
#'    "Aa&Bb", "camelCasePhrases",
#'    "AlphaBetaGamma", "Alpha       Beta", "Beta  !!! Gamma",
#'    "a + b", "a - b", "a * b"
#'    )
#'  )
#'
#'  library(dplyr)
#'  df <- read.csv("data-raw/source_data") %>%
#'    clean_NF_names()
#'
#' @source https://drdoane.com/clean-consistent-column-names/,
#' with some edits by AAM.
clean_NF_names <- function(.data, unique = FALSE) {
  n <- if (is.data.frame(.data)) colnames(.data) else .data

  n <- gsub("%+", "_pct_", n)
  n <- gsub("\\$+", "_dollars_", n)
  n <- gsub("\\++", "_plus_", n)
  n <- gsub("-+", "", n)
  n <- gsub("\\*+", "_star_", n)
  n <- gsub("#+", "_cnt_", n)
  n <- gsub("&+", "_and_", n)
  n <- gsub("@+", "_at_", n)

  n <- gsub("[^a-zA-Z0-9_]+", "_", n)
  # the line below inserts an underscore when a name hasCrunchedSpacingLikeThis.
  # I am removing it so that the scale names (e.g., "somAnx") still work.
  # n <- gsub("([A-Z][a-z])", "_\\1", n)
  n <- trimws(n)

  n <- gsub("(^_+|_+$)", "", n)

  n <- gsub("_+", "_", n)

  # this should move any leading numbers to be trailing numbers,
  # e.g. 1_level should become level_1
  n <- gsub("^([0-9]+)([._])(.*)$", "\\3\\2\\1", n)

  # AAM adding a lowercase converter for Q items
  n <- gsub("^q", "Q", n)
  # and removing the underscores between Q and numbers
  n <- gsub("^Q_", "Q", n)

  if (unique) n <- make.unique(n, sep = "_")

  if (is.data.frame(.data)) {
    colnames(.data) <- n
    .data
  } else {
    n
  }
}

