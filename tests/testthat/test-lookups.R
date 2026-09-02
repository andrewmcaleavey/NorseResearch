test_that("lookup_item supports NF 2.0, 2.1, and 3.1 tables", {
  nf2 <- lookup_item("Q61", version = 2, verbose = TRUE)
  nf21 <- lookup_item("Q61", version = 2.1, verbose = TRUE)
  nf3 <- lookup_item("Q201", version = 3.1, verbose = TRUE)

  expect_s3_class(nf2, "data.frame")
  expect_s3_class(nf21, "data.frame")
  expect_s3_class(nf3, "data.frame")
  expect_equal(nrow(nf2), 1L)
  expect_equal(nrow(nf21), 1L)
  expect_equal(nrow(nf3), 1L)
  expect_type(lookup_item("Q145", version = 2.1), "character")
  expect_equal(lookup_item(c("Q61", "Q115"), version = 2),
               c("I have given up hope for a better future",
                 "No matter how hard I try, things do not get better"))
  expect_error(lookup_item("Q1", version = 4), "Unsupported NF version")
})

test_that("lookup_trigger accepts simple, full, and partial NF2.1 names", {
  expect_equal(lookup_trigger("hopeless"), "Q115")
  expect_equal(lookup_trigger("Hopeless"), "Q115")
  expect_equal(lookup_trigger("hopeless", version = 2), "Q61")
  expect_warning(expect_null(lookup_trigger("definitely-not-a-scale")),
                 "scaleName not recognized")
})

test_that("lookup_trigger_among handles one, many, and no triggers", {
  expect_equal(lookup_trigger_among(c("Q51", "Q46")), "Q51")

  triggers <- c(lookup_trigger("somAnx"), lookup_trigger("sad"))
  expect_message(out <- lookup_trigger_among(triggers), "2 trigger items")
  expect_setequal(out, triggers)

  expect_warning(out_none <- lookup_trigger_among("Q999"), "None of these items")
  expect_identical(out_none, NA_character_)
  expect_equal(lookup_trigger_among(c("Q61", "Q115"), version = 2), "Q61")
})

test_that("packaged lookup and scoring data have their required schemas", {
  expect_true(all(c("item", "item_text_e", "trigger") %in% names(nf2.1.item.descriptions)))
  expect_true(all(c("item", "item_text_e") %in% names(NF3.1_items)))
  expect_true(all(c("ScoreName", "ScaleName") %in% names(scoreNames.nf3)))
  expect_length(names.list, length(scale_names) + 2L)
  expect_true(all(vapply(names.list, is.character, logical(1))))
})
