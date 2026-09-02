test_that("collapse_measures_wide widens matching measures and orders observations", {
  dat <- data.frame(
    Respondent_ID = c(1, 1, 2),
    Submitted = c("2024-01-02", "2024-01-01", "2024-03-01"),
    Short_code = c("M10", "M10", "M20"),
    M10_Q1 = c("2", "1", NA),
    M10_Q2 = c("4", "3", NA),
    M20_Q1 = c(NA, NA, "5"),
    check.names = FALSE
  )

  out <- collapse_measures_wide(dat)

  expect_s3_class(out, "tbl_df")
  expect_named(out, c("Respondent_ID", "Submitted", "Short_code", "Q1", "Q2"))
  expect_equal(out$Submitted, c("2024-01-01", "2024-01-02", "2024-03-01"))
  expect_equal(out$Q1, c("1", "2", "5"))
  expect_equal(out$Q2, c("3", "4", NA))
  expect_true(is.list(attr(out, "q_numeric_coercion")))
})

test_that("collapse_measures_wide prefers higher A suffixes and validates code columns", {
  dat <- data.frame(
    Respondent_ID = 1,
    Submitted = "2024-01-01",
    Short_code = "none",
    value = 1,
    value_A1 = 2,
    value_A2 = 3
  )
  expect_warning(out <- collapse_measures_wide(dat), NA)
  expect_equal(out$value, 3)
  expect_false(any(grepl("_A[0-9]+$", names(out))))
  expect_error(collapse_measures_wide(data.frame(x = 1)), "Neither 'Kortkode' nor 'Short_code'")
})

test_that("collapse_measures_wide can merge Q item suffixes after widening", {
  dat <- data.frame(
    Respondent_ID = c(1, 1),
    Submitted = c("2024-01-01", "2024-01-02"),
    Short_code = c("M10", "M10"),
    M10_Q7 = c(NA, 4),
    M10_Q7_1 = c(2, NA),
    check.names = FALSE
  )
  out <- collapse_measures_wide(dat)
  expect_equal(out$Q7, c(2, 4))
  expect_false("Q7_1" %in% names(out))
})

test_that("fa_table creates gt tables from a one-factor result", {
  fit <- list(
    loadings = structure(matrix(c(.8, .2), ncol = 1,
                                dimnames = list(c("Q1", "Q2"), "F1")),
                         class = "loadings"),
    communality = c(.64, .04),
    uniquenesses = c(.36, .96),
    complexity = c(1, 1),
    Vaccounted = matrix(c(.68, .34, .34), ncol = 1,
                        dimnames = list(c("SS loadings", "Proportion Var", "Cumulative Var"), "F1"))
  )

  out <- suppressWarnings(fa_table(fit, sort = FALSE, title = "Test factors"))
  expect_named(out, c("ind_table", "f_table"))
  expect_s3_class(out$ind_table, "gt_tbl")
  expect_s3_class(out$f_table, "gt_tbl")
})
