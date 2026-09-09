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

test_that("fix_failed_encoding repairs all default mojibake signal families", {
  bad <- c(
    intToUtf8(c(0x00c3, 0x00a5)),
    intToUtf8(c(0x00c3, 0x00b8)),
    intToUtf8(c(0x00c3, 0x00a6)),
    intToUtf8(c(0x00c3, 0x2026)),
    intToUtf8(c(0x00c3, 0x02dc)),
    intToUtf8(c(0x00c3, 0x2020)),
    intToUtf8(c(0x00c2, 0x00a9)),
    intToUtf8(c(0x00e2, 0x20ac, 0x2122)),
    intToUtf8(c(0x00e2, 0x20ac, 0x0153)),
    intToUtf8(c(0x00e2, 0x20ac, 0x009d)),
    intToUtf8(c(0x00e2, 0x20ac, 0x201c)),
    intToUtf8(c(0x00e2, 0x20ac, 0x201d)),
    intToUtf8(c(0x00e2, 0x20ac, 0x00a6))
  )
  expected <- c(
    "\u00e5", "\u00f8", "\u00e6", "\u00c5", "\u00d8", "\u00c6", "\u00a9",
    "\u2019", "\u201c", "\u201d", "\u2013", "\u2014", "\u2026"
  )

  out <- fix_failed_encoding(data.frame(x = bad), report = "none")

  expect_equal(out$x, expected)
  expect_true(all(utf8::utf8_valid(out$x)))
})

test_that("fix_failed_encoding preserves clean and ambiguous text", {
  dat <- data.frame(
    x = c("A\u00f1o", "S\u00e3o Paulo", "M\u00e3nana", "F\u00f8rde", "\u00c3gua"),
    stringsAsFactors = FALSE
  )

  out <- expect_no_error(fix_failed_encoding(dat, report = "none"))

  expect_identical(out$x, dat$x)
  expect_true(all(utf8::utf8_valid(out$x)))
  expect_identical(fix_failed_encoding(out, report = "none"), out)
})

test_that("fix_failed_encoding does not fail on invalid UTF-8 values", {
  # The valid prefix makes sanitize_vec detect the mojibake signal, while the
  # trailing invalid byte exercises the guarded conversion path.
  invalid <- rawToChar(as.raw(c(0xc3, 0x83, 0xc3, 0x28)))
  Encoding(invalid) <- "bytes"
  dat <- data.frame(x = invalid, stringsAsFactors = FALSE)

  out <- expect_no_error(fix_failed_encoding(dat, report = "none"))

  expect_identical(charToRaw(out$x), charToRaw(dat$x))
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
