test_that("mirt information and plotting helpers return coherent results", {
  set.seed(123)
  dat <- as.data.frame(matrix(sample(1:5, 800, replace = TRUE), ncol = 4))
  names(dat) <- paste0("Q", 1:4)
  fit <- suppressWarnings(mirt::mirt(dat, 1, itemtype = "graded", verbose = FALSE))
  theta <- seq(-2, 2, length.out = 11)

  expect_warning(
    legacy <- info_mirt(fit, z = c(-2, 2), printAuto = FALSE),
    "deprecated",
    ignore.case = TRUE
  )
  current <- info(fit, z = c(-2, 2), printAuto = FALSE)

  expect_named(legacy, c("Item", "Info", "PctTot"))
  expect_named(current, c("item", "info", "PctTot"))
  expect_equal(current$item, names(dat))
  expect_equal(sum(current$PctTot), 100, tolerance = .1)

  faceted <- ggplot_icc_plot(fit, Theta = theta, scale.name = "Example")
  separate <- ggplot_icc_plot(fit, Theta = theta, facet_items = FALSE)
  tif <- ggplot_information_plot(fit, Theta = theta, plot_type = "test",
                                 scale.name = "Example")
  iif <- ggplot_information_plot(fit, Theta = theta, plot_type = "items")

  expect_s3_class(faceted, "ggplot")
  expect_length(separate, 4L)
  expect_true(all(vapply(separate, inherits, logical(1), "ggplot")))
  expect_s3_class(tif, "ggplot")
  expect_s3_class(iif, "ggplot")
  expect_match(tif$labels$title, "Example")
})

test_that("generic info supports ltm graded-response models", {
  set.seed(124)
  dat <- as.data.frame(matrix(sample(1:5, 600, replace = TRUE), ncol = 4))
  names(dat) <- paste0("Q", 1:4)
  fit <- suppressWarnings(ltm::grm(dat))

  out <- info(fit, z = c(-3, 3), printAuto = FALSE)

  expect_named(out, c("item", "info", "PctTot"))
  expect_equal(out$item, names(dat))
  expect_true(all(is.finite(out$info)))
  expect_true(any(out$info > 0))
  expect_equal(sum(out$PctTot), 100, tolerance = .1)

  expect_warning(
    legacy <- info_ltm(fit, z = c(-3, 3), printAuto = FALSE),
    "deprecated",
    ignore.case = TRUE
  )
  expect_named(legacy, c("item", "info", "PctTot"))
  expect_type(legacy$info, "double")
})

test_that("scale_analysis2 completes its mirt workflow", {
  set.seed(125)
  dat <- as.data.frame(matrix(sample(1:5, 600, replace = TRUE), ncol = 3))
  names(dat) <- c("Q100", "Q102", "Q42")

  out <- suppressMessages(suppressWarnings(
    scale_analysis2(
      "Sad Affect",
      names(dat),
      dat,
      IRTpackage = "mirt",
      print.now = FALSE,
      version = "3.1"
    )
  ))

  expect_s3_class(out, "scale_analysis2")
  expect_equal(out$items, names(dat))
  expect_equal(out$item_text, lookup_item(names(dat), version = "3.1"))
  expect_true(all(vapply(out[c("histogram", "ICC", "TIF", "IIC")],
                         inherits, logical(1), "ggplot")))
  expect_named(out$info, c("item", "info", "PctTot"))
})
