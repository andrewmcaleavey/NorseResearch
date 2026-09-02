test_that("random item generators respect size, range, and reproducibility", {
  set.seed(42)
  first <- random_item_vector(20)
  set.seed(42)
  second <- random_item_vector(20)

  expect_identical(first, second)
  expect_length(first, 20L)
  expect_true(all(first %in% 1:7))
  expect_true(random_item_response() %in% 1:7)
  expect_setequal(random_item_vector(3, range = 8:10, replace = FALSE), 8:10)
})

test_that("random_person_generator returns one valid demographic row per person", {
  set.seed(1)
  out <- random_person_generator(
    3,
    gender = "test",
    birthyear = 1990,
    tx_focus = "MH",
    in_or_out = "Outpatient"
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(out$anon_id, 1:3)
  expect_true(all(out$pt_total_obs >= 1))
  expect_true(inherits(out$pt_first_date, "Date"))
  expect_true(all(out$gender == "test"))
})

test_that("random_tx_generator creates ordered dates for each identifier", {
  set.seed(2)
  out <- random_tx_generator(
    num_obs = c(2, 3),
    first_date = as.Date(c("2020-01-01", "2020-02-01")),
    identifiers = c("a", "b"),
    tx_days = c(20, 30)
  )

  expect_equal(as.integer(table(out$anon_id)), c(2L, 3L))
  expect_equal(dimnames(table(out$anon_id))[[1]], c("a", "b"))
  expect_equal(out$date[c(1, 3)], as.Date(c("2020-01-01", "2020-02-01")))
  expect_true(all(diff(out$date[out$anon_id == "a"]) > 0))
  expect_true(all(diff(out$date[out$anon_id == "b"]) > 0))
})

test_that("random_assessment_generator adds every NF2 item without changing dates", {
  dates <- tibble::tibble(
    anon_id = c(3L, 3L),
    date = as.Date(c("2021-01-01", "2021-01-08"))
  )
  set.seed(3)
  out <- random_assessment_generator(dates)

  expect_equal(out[c("anon_id", "date")], dates)
  expect_true(all(item_names_nf2 %in% names(out)))
  expect_true(all(unlist(out[item_names_nf2]) %in% 1:7))
})

test_that("random_norse_data joins patient, treatment, and item data", {
  set.seed(4)
  out <- random_norse_data(4)

  expect_s3_class(out, "tbl_df")
  expect_setequal(unique(out$anon_id), 1:4)
  expect_true(all(item_names_nf2 %in% names(out)))
  expect_equal(as.integer(table(out$anon_id)), out$pt_total_obs[match(1:4, out$anon_id)])
})
