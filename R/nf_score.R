# New scoring function: overall scoring regardless of version
# Take raw export data
# clean the names and variables
# combine versions
# ensure reversing is done
# score all relevant versions

#' Scoring mixed NF data
#'
#' This function should take a dataset and add new variables as needed.
#' It is obsolete and should be replaced with `score_all()`.
#'
#' @param dat A data.frame.
#' @param versions_to_score Character vector. Which versions should be scored
#' if present. defaults to `c("2", "3")`, which scores the 2 and 3. Only 2 and 3
#' are available now. Ideally will add versions over time.
#' @param version_variable Character. Name of the variable in `dat` that contains version information.
#' @param process_vars Logical. Should process variables (Alliance, medications) be included? Defaults to `TRUE`.
#' @param raw_scores Logical. Should raw scale scores be computed? Defaults to `TRUE`.
#' @param factor_scores Logical. Should factor-scored scale scores be computed? Defaults to `FALSE`.
#'
#' @return A data.frame/tibble
#' @export
#'
#' @examples
#' /dontrun{
#' nf_score()
#' }
nf_score <- function(dat,
                     versions_to_score = c("2", "3"),
                     version_variable = "Ver_10",
                     process_vars = TRUE,
                     raw_scores = TRUE,
                     factor_scores = FALSE){
  outdat <- dat

  # final steps
  # message to summarize
  print(paste("Scored the NF versions:", versions_to_score, "\n"))
  print(paste("Process variables were included?", process_vars, "\n"))
  print(paste("Computed the raw scores?", raw_scores, "\n"))
  if(factor_scores){print(paste("Computed factor scores."))}
  return(outdat)
}
