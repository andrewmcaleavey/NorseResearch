test_that("check_rev detects correlation direction and validates inputs", {
  x <- rep(1:7, 2)
  nf2 <- data.frame(Q15=x, Q115=x, Q27=x, Q141=x, Q140=x,
                    Q10=x, Q123=x, Q67=x, Q126=x)

  expect_true(check_rev(nf2))
  expect_false(check_rev(transform(nf2, Q15 = 8 - Q15)))
  expect_named(check_rev(nf2, verbose = TRUE),
               c("reversed", "rQ15.Q115", "rQ27.Q141", "rQ140.Q141",
                 "rQ10.Q123", "rQ67.Q126"))
  expect_error(check_rev(nf2, version = "NF4"), "Incorrect version")
  expect_error(check_rev(transform(nf2, Q15 = 8)), "outside scoring range")
})

test_that("score_all_NORSE2 adds the advertised NF2 scale scores", {
  set.seed(10)
  dat <- random_assessment_generator(num_dates = 30)
  out <- suppressWarnings(score_all_NORSE2(dat))

  expected <- c(scale_names, "alliance", "needs")
  expect_true(all(expected %in% names(out)))
  expect_equal(nrow(out), nrow(dat))
  expect_true(all(out$cog >= 1 & out$cog <= 7))
})

test_that("NF2 scale scores match explicit expected means", {
  items <- unique(c(item_names_nf2, ona.names))
  dat <- as.data.frame(
    setNames(lapply(items, function(item) {
      if (identical(item, "Q149")) c(7, 7, 7) else c(1, 3, 5)
    }), items),
    check.names = FALSE
  )

  out <- suppressWarnings(score_all_NORSE2(dat, process_vars = TRUE))
  out_without_process <- suppressWarnings(score_all_NORSE2(dat, process_vars = FALSE))
  expected_ona <- c(32 / 26, 82 / 26, 132 / 26)

  expect_equal(out$cog, c(1, 3, 5))
  expect_equal(out$hopeless, c(1, 3, 5))
  expect_equal(out$sad, c(1, 3, 5))
  expect_equal(out$worry, c(1, 3, 5))
  expect_equal(out$ona, expected_ona)
  expect_equal(out_without_process$ona, expected_ona)
})

test_that("ONA uses the canonical 26 NF2 items", {
  expected_items <- c(
    "Q100", "Q101", "Q102", "Q111", "Q115", "Q117", "Q120", "Q126",
    "Q127", "Q128", "Q141", "Q142", "Q147", "Q149", "Q19", "Q24", "Q3",
    "Q34", "Q38", "Q39", "Q42", "Q51", "Q53", "Q64", "Q75", "Q88"
  )

  expect_identical(ona.names, expected_items)
  expect_identical(ona.names.nf3, expected_items)
})

test_that("score_all_NORSE2_ou adds over-under scores", {
  set.seed(11)
  dat <- random_assessment_generator(num_dates = 10)
  out <- suppressWarnings(score_all_NORSE2_ou(dat))

  expect_true(all(c(scale_names_ou, "alliance_ou", "needs_ou") %in% names(out)))
  expect_equal(nrow(out), nrow(dat))
})

test_that("score_all_nf3 calculates each NF3 score and QOL", {
  object_names <- c(
    "anger.names.nf3", "cog.names.nf3", "eating.names.nf3", "genFunc.names.nf3",
    "hopeless.names.nf3", "impulsivity.names.nf3", "intAvoid.names.nf3",
    "intMem.names.nf3", "pain.names.nf3", "physAnx.names.nf3", "ready.names.nf3",
    "sad.names.nf3", "selfComp.names.nf3", "selfContempt.names.nf3",
    "socAvoid.names.nf3", "socSup.names.nf3", "subUse.names.nf3",
    "suicide.names.nf3", "worry.names.nf3"
  )
  objects <- mget(object_names, inherits = TRUE)
  items <- unique(c(unlist(objects, use.names = FALSE), "Q226"))
  dat <- as.data.frame(matrix(rep(c(1, 3, 5), each = length(items)),
                              nrow = 3, byrow = TRUE), check.names = FALSE)
  names(dat) <- items

  out <- suppressWarnings(score_all_nf3(dat))

  expected <- c(sub("\\.names\\.nf3$", "", object_names), "QOL")
  expect_true(all(expected %in% names(out)))
  expect_equal(out$cog, c(1, 3, 5))
  expect_equal(out$hopeless, c(1, 3, 5))
  expect_equal(out$sad, c(1, 3, 5))
  expect_equal(out$worry, c(1, 3, 5))
  expect_equal(out$ona, c(1, 3, 5))
  expect_equal(out$QOL, c(1, 3, 5))
})

test_that("ONA scores NF3 data using available canonical items", {
  dat <- as.data.frame(
    matrix(rep(c(1, 3, 5), each = nrow(NF3.1_items)),
           nrow = 3, byrow = TRUE),
    check.names = FALSE
  )
  names(dat) <- NF3.1_items$item
  dat$Q226 <- c(1, 3, 5)

  out <- suppressWarnings(score_all_nf3(dat))

  expect_equal(out$ona, c(1, 3, 5))
  expect_equal(length(intersect(ona.names.nf3, names(dat))), 23L)
})

test_that("score_normed_NF uses bundled norms and preserves source scores", {
  norm_mean <- summary_norms_MH_out$cog[[1]]
  norm_sd <- summary_norms_MH_out$cog[[2]]
  dat <- tibble::tibble(cog = c(norm_mean, norm_mean + norm_sd))
  out <- score_normed_NF(dat, cog)
  expect_equal(out$cog, dat$cog)
  expect_equal(out$cog_normed_MHout_Z, c(0, 1), tolerance = 1e-8)
})

test_that("score_all scores mixed NF2/NF3 rows and applies special missing codes", {
  nf3_object_names <- c(
    "anger.names.nf3", "cog.names.nf3", "eating.names.nf3", "genFunc.names.nf3",
    "hopeless.names.nf3", "impulsivity.names.nf3", "intAvoid.names.nf3",
    "intMem.names.nf3", "pain.names.nf3", "physAnx.names.nf3", "ready.names.nf3",
    "sad.names.nf3", "selfComp.names.nf3", "selfContempt.names.nf3",
    "socAvoid.names.nf3", "socSup.names.nf3", "subUse.names.nf3",
    "suicide.names.nf3", "worry.names.nf3"
  )
  nf3_object_names <- c(nf3_object_names, "alliance.names.nf3", "pref.names.nf3")
  nf3_items <- unlist(mget(nf3_object_names, inherits = TRUE), use.names = FALSE)
  items <- unique(c(item_names_nf2, nf3_items, "Q226"))
  dat <- as.data.frame(matrix(2, nrow = 2, ncol = length(items)), check.names = FALSE)
  names(dat) <- items
  dat$Ver_10 <- c("2.1", "3.1")
  dat[2, cog.names.nf3] <- -98

  out <- suppressWarnings(score_all(dat, process_vars = TRUE))

  expect_equal(out$cog, c(2, 1))
  expect_true(is.na(out$anger[[1]]))
  expect_equal(out$anger[[2]], 2)
  expect_equal(out$ona, c(2, 50 / 26))
  expect_equal(out$QOL, dat$Q226)
})
