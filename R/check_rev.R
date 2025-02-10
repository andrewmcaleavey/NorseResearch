#' check if NORSE scores are reversed
#'
#' @param .data The data to clean.
#' @param verbose Logical. Default is FALSE. If TRUE, will
#' return the list of correlations tested with their values.
#' @param version Which version of NF is being used. Acceptable values
#' are "NF2" and "NF3".
#'
#' @return logical. Is data likely reversed? TRUE if yes.
#' If \code{verbose} is \code{TRUE}, returns a named list.
#' @export
#'
#' @examples
#' check_rev(data.joined)
#' check_rev(data.joined, verbose = TRUE)
#'
check_rev <- function(.data, verbose = FALSE, version = "NF2") {
  # check to see if all values are between 1 and 7 first
  qs <- .data %>%
    select(matches("^Q\\d+"))   %>%
    select(-matches("2[2]"))
  try(qs <- select(qs, -Q71, -Q72, -Q152, -Q153, -Q74),
      silent = TRUE)


  if(any(qs < 1 | qs > 7, na.rm = TRUE)){
    stop("Some NF values outside scoring range. Check that all NA values
  are properly coded and all item responses are between 1 and 7.")
  }
  if(version == "NF2"){
    val1 <- with(.data, cor(Q15, Q115, use = "complete.obs"))
    val2 <- with(.data, cor(Q27, Q141, use = "complete.obs"))
    val3 <- with(.data, cor(Q140, Q141, use = "complete.obs"))
    val4 <- with(.data, cor(Q10, Q123, use = "complete.obs"))
    val5 <- with(.data, cor(Q67, Q126, use = "complete.obs"))

    if(!verbose){
      output <- ifelse(any(mget(ls(pattern = "val")) < 0),
                       FALSE,
                       TRUE)
    } else {
      output = list("reversed" = ifelse(any(mget(ls(pattern = "val")) < 0),
                                        FALSE,
                                        TRUE),
                    "rQ15.Q115" = val1,
                    "rQ27.Q141" = val2,
                    "rQ140.Q141" = val3,
                    "rQ10.Q123" = val4,
                    "rQ67.Q126" = val5)
    }
  } else if(version == "NF3"){
    val1 <- with(.data, cor(Q202, Q215, use = "complete.obs"))
    # val2 <- with(.data, cor(Q204, Q141, use = "complete.obs"))
    # social support is all reversed, no obvious opposites.
    val3 <- with(.data, cor(Q140.1, Q207, use = "complete.obs"))
    # val4 <- with(.data, cor(Q43, Q123, use = "complete.obs"))
    val5 <- with(.data, cor(Q211, Q215, use = "complete.obs"))
    # val6 <- with(.data, cor(Q212, Q126, use = "complete.obs"))
    val7 <- with(.data, cor(Q217, Q207, use = "complete.obs"))
    # val8 <- with(.data, cor(Q222, Q141, use = "complete.obs"))
    val9 <- with(.data, cor(Q223, Q205, use = "complete.obs"))
    val10 <- with(.data, cor(Q220, Q207, use = "complete.obs"))
    # val11 <- with(.data, cor(Q221, Q126, use = "complete.obs"))
    # val12 <- with(.data, cor(Q84, Q126, use = "complete.obs"))

    if(!verbose){
      output <- ifelse(any(mget(ls(pattern = "val")) < 0),
                       FALSE,
                       TRUE)
    } else {
      output = list("reversed" = ifelse(any(mget(ls(pattern = "val")) < 0),
                                        FALSE,
                                        TRUE),
                    "rQ202.Q215" = val1,
                    "rQ140.Q207" = val3,
                    "rQ211.Q215" = val5,
                    "rQ217.Q207" = val7,
                    "rQ223.Q225" = val9,
                    "rQ220.Q207" = val10)
    }
  } else if(!version %in% c("NF2", "NF3")){
    stop("Incorrect version provided. Only use either NF2 or NF3")
  }
  return(output)
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
