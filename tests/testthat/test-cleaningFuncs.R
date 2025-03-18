test_that("get_nicer_name() works with NF2", {
  expect_equal(get_nicer_name("eating"),
               "Eating Problems")
  expect_equal(get_nicer_name(c("eating", "subUse")),
               c("Eating Problems", "Substance Use"))
})

test_that("get_nicer_name() returns message and simplename when no match found", {
  testthat::expect_message(get_nicer_name("foo"))
  expect_equal(quietly(get_nicer_name)("foo")[[1]], "foo")
  testthat::expect_message(get_nicer_name(c("sad", "foo")))
  expect_equal(quietly(get_nicer_name)(c("sad", "foo"))[[1]],
               c("Sad Affect", "foo"))
})

# get_nf3_nicer_name needs tests too

test_that("get_nf3_nicer_name() works with NF3", {
  expect_equal(get_nf3_nicer_name("eating"),
               "Restrictive Eating")
  expect_equal(get_nf3_nicer_name(c("eating", "subUse")),
               c("Restrictive Eating", "Substance Use"))
})

test_that("get_nf3_nicer_name() returns message and simplename when no match found", {
  testthat::expect_message(get_nf3_nicer_name("foo"))
  expect_equal(quietly(get_nf3_nicer_name)("foo")[[1]], "foo")
  testthat::expect_message(get_nf3_nicer_name(c("sad", "foo")))
  expect_equal(quietly(get_nf3_nicer_name)(c("sad", "foo"))[[1]],
               c("Sad Affect", "foo"))
})


context("combine_q_vars")

test_that("combine_q_vars combines paired Q variables correctly", {
  df <- data.frame(
    Q140 = c(NA, 2, NA),
    Q140_1 = c(1, NA, 3),
    Q150 = c(4, NA, 6),
    Q150_1 = c(NA, 5, NA),
    stringsAsFactors = FALSE
  )

  # Expected: Q140 should become c(1, 2, 3) and Q150 should become c(4, 5, 6).
  df_combined <- combine_q_vars(df)

  expect_false("Q140_1" %in% names(df_combined))
  expect_false("Q150_1" %in% names(df_combined))
  expect_equal(df_combined$Q140, c(1, 2, 3))
  expect_equal(df_combined$Q150, c(4, 5, 6))
})

test_that("combine_q_vars errors on conflict", {
  df_conflict <- data.frame(
    Q140 = c(1, NA),
    Q140_1 = c(2, 3),
    stringsAsFactors = FALSE
  )

  expect_error(combine_q_vars(df_conflict), "Conflict")
})
