# test-read_password_protected_excel.R

test_that("function throws error if files and passwords lengths differ", {
  # Expect an error due to length mismatch
  expect_error(
    read_password_protected_excel(
      files = c("file1.xlsx", "file2.xlsx"),
      passwords = "mysecret"
    ),
    regexp = "The length of 'files' must match the length of 'passwords'."
  )
})

test_that("function returns a single data frame if one file is provided", {
  # Skip if dependencies not installed or can't run
  skip_if_not_installed("reticulate")
  skip_if_not_installed("readxl")

  print(reticulate::py_config())
  # Also skip if Python environment/msoffcrypto isn't set up
  # We'll do a simple check for reticulate's ability to run Python code
  skip_if(isFALSE(reticulate::py_available(initialize = FALSE)),
          "No suitable Python environment available.")

  # (Optional) skip this test on CRAN or CI to avoid the overhead
  # skip_on_cran()
  # skip_on_ci()

  # In a real scenario, you would have a small, password-protected test file
  # stored in tests/testthat/testdata/password_protected.xlsx
  # along with a known password.
  #
  # For demonstration, let's just skip if that file doesn't exist.
  test_file <- testthat::test_path("testdata", "password_protected.xlsx")
  if (!file.exists(test_file)) {
    skip("No test file available for decryption test.")
  }

  # Provide correct password
  correct_password <- "mysecret"

  # Call the function
  df <- read_password_protected_excel(
    files = test_file,
    passwords = correct_password
  )

  # Check that the result is a data frame (tibble inherits from data.frame)
  expect_s3_class(df, "data.frame")
  # Optionally, check that it has some expected columns
  # expect_true(all(c("colA", "colB") %in% names(df)))
})

test_that("function returns a named list of data frames if multiple files are provided", {
  skip_if_not_installed("reticulate")
  skip_if_not_installed("readxl")
  skip_if(isFALSE(reticulate::py_available(initialize = FALSE)),
          "No suitable Python environment available.")

  # Suppose we have two password-protected files in testdata/ directory
  test_file1 <- testthat::test_path("testdata", "file1.xlsx")
  test_file2 <- testthat::test_path("testdata", "file2.xlsx")
  if (!all(file.exists(c(test_file1, test_file2)))) {
    skip("Not all test files available for decryption test.")
  }

  # Provide correct passwords
  pw1 <- "secret1"
  pw2 <- "secret2"

  results_list <- read_password_protected_excel(
    files = c(test_file1, test_file2),
    passwords = c(pw1, pw2)
  )

  # Expect a list of length 2
  expect_length(results_list, 2)
  # Each element should be a data frame
  purrr::walk(results_list, ~ expect_s3_class(.x, "data.frame"))
  # Names should match file names (minus extension)
  expected_names <- c(
    tools::file_path_sans_ext(basename(test_file1)),
    tools::file_path_sans_ext(basename(test_file2))
  )
  expect_equal(names(results_list), expected_names)
})

test_that("function returns error if decryption fails", {
  skip_if_not_installed("reticulate")
  skip_if_not_installed("readxl")
  skip_if(isFALSE(reticulate::py_available(initialize = FALSE)),
          "No suitable Python environment available.")

  # We have a test file, but the wrong password
  test_file <- testthat::test_path("testdata", "password_protected.xlsx")
  if (!file.exists(test_file)) {
    skip("No test file available for decryption test.")
  }

  # Wrong password
  wrong_password <- "not_the_right_password"

  # We expect a Python-side error due to invalid password
  # This may throw a somewhat different error message.
  # Adjust the regexp if needed based on the actual error message
  expect_error(
    read_password_protected_excel(test_file, wrong_password),
    regexp = "InvalidKeyError"
  )

})
