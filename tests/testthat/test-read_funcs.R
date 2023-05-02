# to test these functions need external files that contain -98 and -99.

# this is what the data looked like:
# toydata <- tibble::tibble("A" = c(1, 2, 1, 2, 1, 2, 1, 2),
#                           "B" = c(3, 4, 3, 4, 3, 4, 3, 4),
#                           "C" = c(5, 6, -98, NA, -99, 6, 5, 6))
# write.csv(toydata,
#           file = "tests/testthat/test-helpers/toydata.csv",
#           row.names = FALSE)
# write.csv2(toydata,
#            file = "tests/testthat/test-helpers/toydata2.csv",
#            row.names = FALSE)
# # also made toydata.xlsx separately.

toydata_csv <- read.csv("tests/testthat/test-helpers/toydata.csv")
toydata_csv2 <- read.csv2("tests/testthat/test-helpers/toydata2.csv")
toydata_xlsx <- readxl::read_excel("tests/testthat/test-helpers/toydata.xlsx")


testthat::test_that("read.csv_nf3() correctly interprets -99 as missing", {
  testthat::expect_true(is.na(read.csv_nf3("tests/testthat/test-helpers/toydata.csv")[5, 3]))
}
)

testthat::test_that("read.csv_nf3() returns a data.frame", {
  testthat::expect_true(is.data.frame(read.csv_nf3("tests/testthat/test-helpers/toydata.csv")))
})

testthat::test_that("read.csv2_nf3() correctly interprets -99 as missing", {
  testthat::expect_true(is.na(read.csv2_nf3("tests/testthat/test-helpers/toydata2.csv")[5, 3]))
}
)

testthat::test_that("read.csv2_nf3() returns a data.frame", {
  testthat::expect_true(is.data.frame(read.csv2_nf3("tests/testthat/test-helpers/toydata2.csv")))
})
