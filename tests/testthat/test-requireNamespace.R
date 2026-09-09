test_that("core data are available from the namespace", {
  ns <- asNamespace("NorseResearch")
  internal_objects <- c(
    "anger.names.nf3", "cog.names.nf3", "nf2.1.item.descriptions",
    "nf2.1.logic", "NF3.1_items", "scoreNames.nf3", "summary_norms_MH_out"
  )

  expect_true(all(vapply(
    internal_objects,
    exists,
    logical(1),
    envir = ns,
    inherits = FALSE
  )))
})

test_that("scoring works with requireNamespace and no package attachment", {
  package_was_attached <- "package:NorseResearch" %in% search()
  if (package_was_attached) {
    detach("package:NorseResearch", unload = FALSE)
    on.exit(suppressPackageStartupMessages(library(NorseResearch)), add = TRUE)
  }

  expect_true(requireNamespace("NorseResearch", quietly = TRUE))
  expect_false("package:NorseResearch" %in% search())

  set.seed(123)
  dat3 <- NorseResearch::random_norse_data(
    num_ppl = 3,
    versions = "3",
    num_obs = 1
  )
  item_columns <- grep("^Q", names(dat3), value = TRUE)
  dat3[item_columns] <- lapply(dat3[item_columns], function(x) {
    if (is.numeric(x)) rep(3, length(x)) else x
  })

  out3 <- NorseResearch::score_all_nf3(dat3)
  expect_true(all(c("anger", "cog", "sad", "ona", "QOL") %in% names(out3)))
  expect_equal(out3$cog, rep(3, nrow(out3)))
  expect_equal(out3$anger, rep(3, nrow(out3)))

  dat2 <- NorseResearch::random_assessment_generator(num_dates = 30)
  item_columns <- grep("^Q", names(dat2), value = TRUE)
  dat2[item_columns] <- lapply(dat2[item_columns], function(x) {
    if (is.numeric(x)) rep(c(1, 3, 5), length.out = length(x)) else x
  })

  out2 <- suppressWarnings(NorseResearch::score_all_NORSE2(dat2))
  expect_true(all(c("cog", "sad", "ona") %in% names(out2)))
  expect_equal(out2$cog, rep(c(1, 3, 5), length.out = nrow(out2)))

  dat_mixed <- NorseResearch::random_norse_data(
    num_ppl = 6,
    versions = c("2", "3"),
    num_obs = 2
  )
  item_columns <- grep("^Q", names(dat_mixed), value = TRUE)
  dat_mixed[item_columns] <- lapply(dat_mixed[item_columns], function(x) {
    if (is.numeric(x)) rep(3, length(x)) else x
  })

  out_mixed <- suppressWarnings(NorseResearch::score_all(dat_mixed))
  expect_true(all(c("cog", "anger", "QOL", "alliance") %in% names(out_mixed)))
})
