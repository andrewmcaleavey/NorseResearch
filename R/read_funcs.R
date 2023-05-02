# data import functions? NOT just this.
# Need to read in as they are, then reverse, then
# replace the -98 with 1
# experimental: can I create functions that automatically treat
# -98 as 1 and -99 as NA?

#' Read external NF3 data
#'
#' @param file Location of the file
#' @param ... Additional arguments
#'
#' @return
#' @export
#'
#' @examples
#' read.csv_nf3("HF_research_data.csv")
read.csv_nf3 <- function(file, ...){
  output <- read.csv(file = file, header = TRUE, sep = ",", quote = "\"",
           dec = ".", fill = TRUE, comment.char = "",
           na.strings = c("NA", "", "-99"), ...)
  if(check_rev(output, version = "NF3")){

  }
}

#' Read external NF3 data
#'
#' @param file Location of the file
#' @param ... Additional arguments
#'
#' @return
#' @export
#'
#' @examples
#' read.csv_nf3("HF_research_data.csv")
read.csv2_nf3 <- function(file, ...){
  read.csv2(file = file, header = TRUE, sep = ";", quote = "\"",
           dec = ",", fill = TRUE, comment.char = "",
           na.strings = c("NA", "", "-99"), ...)
}

#' Read external NF3 data
#'
#' @param file Location of the file
#' @param ... Additional arguments
#'
#' @return
#' @export
#'
#' @examples
#'
read_excel_nf3 <- function(file, ...){
  readxl::read_excel(path = file,
                     na = c("NA", "", "-99"))
}
