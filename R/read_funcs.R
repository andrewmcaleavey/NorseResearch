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
#' @return a data.frame
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
#' @return a data frame
#' @export
#'
#' @examples
#' read.csv2_nf3("HF_research_data.csv")
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
#' @return a data.frame
#' @export
#'
#' @examples
#' read_excel_nf3("HF_research_data.csv")
read_excel_nf3 <- function(file, ...){
  readxl::read_excel(path = file,
                     na = c("NA", "", "-99"))
}


# read in generic password-protected data from xlse files

#' Read Excel Files Protected by Password
#'
#' This function reads one or more password-protected Excel (.xlsx) files into R.
#' It uses Python (via the \code{reticulate} package) and the \code{msoffcrypto} library
#' to decrypt the files before importing them with the \code{readxl} package.
#'
#' @param files A character vector of file paths to Excel files. If only one file is provided,
#'   it may be a single string.
#' @param passwords A character vector of passwords. Must be the same length as \code{files}.
#'
#' @details
#' This function creates a temporary decrypted copy of each file, using Python's
#' \code{msoffcrypto} library, and then imports the data via \code{readxl::read_excel}.
#' The temporary files are removed automatically by R's internal cleanup process.
#'
#' If only a single file and password are given, the function returns a single data
#' frame (or tibble). If multiple files and passwords are provided, it returns a
#' named list of data frames (tibbles), where each element's name corresponds to the
#' base file name (minus the file extension).
#'
#' @return A single data frame (tibble) if \code{files} has length 1, otherwise a named
#' list of data frames (tibbles).
#'
#' @examples
#' \dontrun{
#' # Single file usage:
#' df <- read_password_protected_excel("secret.xlsx", "myPassword123")
#'
#' # Multiple files usage:
#' file_vec <- c("secret1.xlsx", "secret2.xlsx")
#' pw_vec   <- c("pw1", "pw2")
#' results_list <- read_password_protected_excel(file_vec, pw_vec)
#' }
#'
#' @export
read_password_protected_excel <- function(files, passwords) {

  # Ensure both arguments are vectors of matching length
  if (length(files) != length(passwords)) {
    stop("The length of 'files' must match the length of 'passwords'.")
  }

  # If user provides single file & password, handle as vectors of length 1
  if (is.character(files) && is.character(passwords) &&
      length(files) == 1 && length(passwords) == 1) {
    files <- c(files)
    passwords <- c(passwords)
  }

  results <- list()

  # Loop over file/password pairs
  for (i in seq_along(files)) {
    file <- files[i]
    password <- passwords[i]

    # Temporary file to store decrypted workbook
    tmp_file <- tempfile(fileext = ".xlsx")

    # Decrypt the file using Python and msoffcrypto
    py_script <- sprintf("
import msoffcrypto
with open(r'%s', 'rb') as f_in:
    office_file = msoffcrypto.OfficeFile(f_in)
    office_file.load_key(password='%s')
    with open(r'%s', 'wb') as f_out:
        office_file.decrypt(f_out)
", file, password, tmp_file)

    # Run the Python script
    reticulate::py_run_string(py_script)

    # Read the decrypted Excel file
    data <- readxl::read_excel(tmp_file)

    # Use file name (sans extension) as the list element name
    obj_name <- tools::file_path_sans_ext(basename(file))
    results[[obj_name]] <- data
  }

  # Return a single data.frame/tibble if only one file was read
  if (length(results) == 1) {
    return(results[[1]])
  } else {
    return(results)
  }
}
