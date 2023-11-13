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
