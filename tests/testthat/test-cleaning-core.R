test_that("clean_NF_names cleans vectors and data-frame names", {
  raw <- c(" q 12 ", "GDP (%)", "a & b", "1_level", "camelCase")
  expected <- c("Q12", "GDP_pct", "a_and_b", "level_1", "camelCase")

  expect_equal(clean_NF_names(raw), expected)

  dat <- stats::setNames(data.frame(matrix(1:10, ncol = 5)), raw)
  expect_equal(names(clean_NF_names(dat)), expected)
  expect_equal(clean_NF_names(c("a", "a"), unique = TRUE), c("a", "a_1"))
})

test_that("nicer_id_var accepts unquoted and character column names", {
  dat <- tibble::tibble(id = c("a", "b"), value = 1:2)

  expect_equal(nicer_id_var(dat, id)$patient_id, dat$id)
  expect_equal(nicer_id_var(dat, "id")$patient_id, dat$id)
  expect_named(nicer_id_var(dat, id, keep_old_vars = FALSE), c("value", "patient_id"))
  expect_error(nicer_id_var(dat, missing_id), "id variable not found")
})

test_that("get_first_obs retains the first row for each id", {
  dat <- tibble::tibble(id = c("b", "a", "b", "a"), value = 1:4)

  out_character <- get_first_obs(dat, "id")
  out_symbol <- get_first_obs(dat, id)

  expect_equal(out_character$value, c(2L, 1L))
  expect_equal(out_symbol, out_character)
  expect_false(dplyr::is_grouped_df(out_character))
})

test_that("replace_98s_99s and drop_variable preserve ordinary values", {
  dat <- tibble::tibble(a = c(1, -98, -99), b = c(4, 5, 6))
  out <- replace_98s_99s(dat)

  expect_equal(out$a, c(1, 1, NA))
  expect_equal(out$b, dat$b)
  expect_named(drop_variable(out, "b"), "a")
  expect_equal(drop_variable(out, "not_present"), out)
})

test_that("fix_failed_encoding repairs mojibake only in selected character columns", {
  dat <- data.frame(
    org = c("Helse FÃ¸rde", "Helse Vest"),
    untouched = c("smÃ¥", "OK"),
    number = 1:2,
    stringsAsFactors = FALSE
  )

  out <- fix_failed_encoding(dat, cols = "org", report = "none")

  expect_equal(out$org, c("Helse Førde", "Helse Vest"))
  expect_equal(out$untouched, dat$untouched)
  expect_identical(out$number, dat$number)
  expect_error(fix_failed_encoding("not a data frame"), "must be a data.frame")
})

test_that("make_english_export renames known fields without overwriting targets", {
  dat <- data.frame(Pasientid = 1:2, Skjema = c("A", "B"), Varighet = 3:4)
  out <- make_english_export(dat)
  expect_named(out, c("Respondent_ID", "Measure_name", "Duration"))

  with_target <- data.frame(Pasientid = 1:2, Respondent_ID = c("x", "y"))
  expect_named(make_english_export(with_target), c("Pasientid", "Respondent_ID"))
})

test_that("rename_score_vars validates mappings and makes duplicate names unique", {
  mapping <- data.frame(
    ScoreName = c("S1", "S2"),
    ScaleName = c("Same scale", "Same scale")
  )
  out <- rename_score_vars(data.frame(S1 = 1, S2 = 2), mapping = mapping)

  expect_named(out, c("score_Same_scale", "score_Same_scale.1"))
  expect_error(rename_score_vars(data.frame(S1 = 1), mapping = data.frame(x = 1)),
               "Mapping must have columns")
})
