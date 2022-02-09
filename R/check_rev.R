#' check if NORSE scores are reversed
#'
#' @param .data The data to clean.
#' @param verbose Logical. Default is FALSE. If TRUE, will
#' return the list of correlations tested with their values.
#'
#' @return logical. Is data likely reversed? TRUE if yes.
#' If \code{verbose} is \code{TRUE}, returns a named list.
#' @export
#'
#' @examples
#' check_rev(data.joined)
#' check_rev(data.joined, verbose = TRUE)
#'
check_rev <- function(.data, verbose = FALSE) {
  # check to see if all values are between 1 and 7 first
  qs <- .data %>% select(starts_with("Q", ignore.case = FALSE))
  try(qs <- select(qs, -Q71, -Q72, -Q152, -Q153, -Q74),
      silent = TRUE)


  if(any(qs < 1 | qs > 7, na.rm = TRUE)){
    stop("Some values outside scoring range. Check that all NA values
  are properly coded and all item responses are between 1 and 7.")
  }

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
  return(output)
}

