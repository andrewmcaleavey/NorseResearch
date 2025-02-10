test_that("nf_score() returns data frame", {
  expect_true(is.data.frame(nf_score(HF_research_data_2021)))
})

test_that("nf_score() returns something plausibly correct.", {
  expect_true(ncol(nf_score(HF_research_data_2021)) > ncol(HF_research_data_2021))
})
