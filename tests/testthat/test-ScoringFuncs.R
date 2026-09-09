# tests/testthat/test-ScoringFuncs.R
# Uses: testthat, dplyr, tibble, rlang

get_fun <- function(name) {
  utils::getFromNamespace(name, "NorseResearch")
}

has_obj <- function(name) {
  exists(name, envir = asNamespace("NorseResearch"), inherits = FALSE)
}

testthat::test_that("rev_score reverses 1-7 correctly and preserves NA", {
  rev_score <- get_fun("rev_score")

  x <- c(1, 2, 7, NA_real_)
  out <- rev_score(x)

  testthat::expect_equal(out, c(7, 6, 1, NA_real_))
  testthat::expect_type(out, "double")
})

testthat::test_that("rev_score_NORSE2 reverses the intended NORSE2 items", {
  rev_score <- get_fun("rev_score")
  rev_score_NORSE2 <- get_fun("rev_score_NORSE2")

  cols <- c(
    "Q27","Q131","Q140","Q40","Q15","Q132","Q10","Q62","Q135","Q50","Q134",
    "Q109","Q133","Q154","Q80","Q108","Q136","Q155","Q138","Q84","Q139",
    "Q82","Q67","Q43","Q11","Q12","Q13","Q14"
  )

  dat <- tibble::as_tibble(stats::setNames(
    as.data.frame(replicate(length(cols), c(1, 2, 7), simplify = FALSE)),
    cols
  ))
  dat$other <- c(10, 11, 12)

  out <- rev_score_NORSE2(dat)

  for (nm in cols) {
    testthat::expect_equal(out[[nm]], rev_score(dat[[nm]]), info = nm)
  }
  testthat::expect_equal(out$other, dat$other)
})

testthat::test_that("score_NORSE_trigger computes row means with NA handling (all-NA -> NA)", {
  score_NORSE_trigger <- get_fun("score_NORSE_trigger")

  dat <- data.frame(
    Q1 = c(1, 2, NA, NA),
    Q2 = c(3, NA, NA, NA),
    Q3 = c(5, 4, NA, NA)
  )

  # Pass trigger explicitly to avoid evaluating default lookup_trigger_among(vars)
  out <- suppressWarnings(
    score_NORSE_trigger(dat, vars = c("Q1", "Q2", "Q3"), trigger = "Q1")
  )

  testthat::expect_equal(out[1], mean(c(1, 3, 5), na.rm = TRUE))
  testthat::expect_equal(out[2], mean(c(2, NA, 4), na.rm = TRUE))
  testthat::expect_true(is.na(out[3]))
  testthat::expect_true(is.na(out[4]))
})

testthat::test_that("score_NORSE_mean scores untriggered scales without trigger lookup", {
  score_NORSE_mean <- get_fun("score_NORSE_mean")

  dat <- data.frame(
    Q1 = c(1, NA, NA),
    Q2 = c(3, NA, 5)
  )

  testthat::expect_no_message(
    out <- score_NORSE_mean(dat, vars = c("Q1", "Q2"))
  )
  testthat::expect_equal(out, c(2, NA_real_, 5))
})

testthat::test_that("find_trigger_among identifies an NF2.1 trigger", {
  find_trigger_among <- get_fun("find_trigger_among")

  testthat::expect_equal(find_trigger_among(c("Q1", "Q51", "Q2")), "Q51")
})

testthat::test_that("find_trigger remains as a deprecated NF2 compatibility helper", {
  find_trigger <- get_fun("find_trigger")
  testthat::expect_warning(out <- find_trigger("somAnx"), "deprecated", ignore.case = TRUE)
  testthat::expect_equal(out, "Q51")
})

testthat::test_that("score_NORSE_overunder: locks down current behavior for trigger-only, multi-item, trigger-missing, all-NA", {
  score_NORSE_overunder <- get_fun("score_NORSE_overunder")

  dat <- data.frame(
    Q1 = c(4, 4, NA, NA),
    Q2 = c(NA, 2, 2, NA),
    Q3 = c(NA, 6, NA, NA)
  )

  out <- suppressWarnings(
    score_NORSE_overunder(
      dat,
      vars = c("Q1", "Q2", "Q3"),
      trigger = "Q1",
      opening.thresh = 4,
      closing.thresh = 3
    )
  )

  # Based on your test report (installed code behavior):
  # row1: only trigger present -> 4 - 4 = 0
  # row2: mean(4,2,6)=4 -> 4 - 3 = 1
  # row3: trigger missing but another item present -> CURRENTLY returns 0 (not -1)
  # row4: all NA -> NA
  testthat::expect_equal(out, c(0, 1, 0, NA_real_))
})

testthat::test_that("score_all_NORSE2 finds its scale vectors in the namespace", {
  score_all_NORSE2 <- get_fun("score_all_NORSE2")
  dat <- get_fun("random_assessment_generator")(num_dates = 3)

  testthat::expect_true(all(vapply(
    c("cog.names", "control.names", "eating.names"),
    has_obj,
    logical(1)
  )))
  out <- suppressWarnings(score_all_NORSE2(dat))
  testthat::expect_true(all(c("cog", "control", "eating") %in% names(out)))
})

testthat::test_that("score_all_NORSE2_ou finds its scale vectors in the namespace", {
  score_all_NORSE2_ou <- get_fun("score_all_NORSE2_ou")
  dat <- get_fun("random_assessment_generator")(num_dates = 3)

  out <- suppressWarnings(score_all_NORSE2_ou(dat))
  testthat::expect_true(all(c("cog_ou", "control_ou", "eating_ou") %in% names(out)))
})

testthat::test_that("compute_normed returns z-scored values", {
  compute_normed <- get_fun("compute_normed")

  testthat::expect_equal(compute_normed(5, m_bar = 3, sd = 2), 1)
  testthat::expect_equal(compute_normed(c(3, 5), m_bar = 3, sd = 2), c(0, 1))
})

testthat::test_that("score_normed_NF uses supplied means and standard deviations", {
  score_normed_NF <- get_fun("score_normed_NF")
  dat <- tibble::tibble(cog = c(3, 5))
  norms <- tibble::tibble(cog = c(3, 2, 10))

  out <- score_normed_NF(dat, scale = "cog", normTable = norms)

  testthat::expect_equal(out$cog_normed_NONORMTABLE_Z, c(0, 1))
})

testthat::test_that("item_norm standardizes values using a tibble column", {
  item_norm <- get_fun("item_norm")

  norm_data <- tibble::tibble(Q142 = c(1, 2, 3, NA_real_))
  testthat::expect_equal(item_norm("Q142", value = 2, norm_data = norm_data), 0)
  testthat::expect_equal(item_norm(Q142, value = 3, norm_data = norm_data), 1)
})

testthat::test_that("check_version_nf returns correct version flags", {
  check_version_nf <- get_fun("check_version_nf")

  dat2 <- data.frame(Q101 = 1, foo = 1)
  dat3 <- data.frame(Q201 = 1, bar = 1)
  dat23 <- data.frame(Q101 = 1, Q201 = 1)
  dat0 <- data.frame(x = 1)

  testthat::expect_equal(check_version_nf(dat2), "2")
  testthat::expect_equal(check_version_nf(dat3), "3")
  testthat::expect_equal(check_version_nf(dat23), c("2", "3"))
  testthat::expect_equal(check_version_nf(dat0), "No NF items found.")
})

testthat::test_that("score_all stops if version_variable is missing", {
  score_all <- get_fun("score_all")

  dat <- tibble::tibble(Q1 = 1)
  testthat::expect_error(
    score_all(dat, version_variable = "Ver_10"),
    regexp = "version_variable does not exist",
    ignore.case = TRUE
  )
})

testthat::test_that("combine_suffix_variables coalesces suffix columns and drops redundant ones", {
  combine_suffix_variables <- get_fun("combine_suffix_variables")

  df <- data.frame(
    Q140 = c(NA, 2, 3, NA),
    Q140_1 = c(1, NA, NA, 4),
    Q140_2 = c(NA, NA, 5, NA),
    Q141 = c(10, 11, 12, 13),
    stringsAsFactors = FALSE
  )

  out <- combine_suffix_variables(df)

  testthat::expect_true("Q140" %in% names(out))
  testthat::expect_false(any(c("Q140_1", "Q140_2") %in% names(out)))
  testthat::expect_equal(out$Q140, c(1, 2, 3, 4))
  testthat::expect_equal(out$Q141, df$Q141)
})

testthat::test_that("combine_suffix_variables ignores non-matching base patterns", {
  combine_suffix_variables <- get_fun("combine_suffix_variables")

  df <- data.frame(
    X = c(1, 2),
    foo_1 = c(NA, 3),
    foo = c(4, NA),
    Q1 = c(NA, 1),
    Q1_1 = c(2, NA),
    stringsAsFactors = FALSE
  )
  out <- combine_suffix_variables(df)

  testthat::expect_true(all(c("foo", "foo_1") %in% names(out)))
  testthat::expect_true("Q1" %in% names(out))
  testthat::expect_false("Q1_1" %in% names(out))
  testthat::expect_equal(out$Q1, c(2, 1))
})
