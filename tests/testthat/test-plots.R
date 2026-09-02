test_that("item_plot and scale_plot return labelled ggplots", {
  dat <- tibble::tibble(Q1 = c(1, 2, 2, NA), Q2 = c(2, 3, 4, 5))

  item <- suppressWarnings(item_plot("Q1", data = dat))
  scale <- scale_plot(dat, c("Q1", "Q2"), "Example scale")

  expect_s3_class(item, "ggplot")
  expect_s3_class(scale, "ggplot")
  expect_equal(item$labels$title, "Q1 responses")
  expect_equal(scale$labels$title, "Example scale")
  expect_equal(scale$data$y.mean, c(1.5, 2.5, 3, 5))
})

test_that("scale_plot preserves an all-missing row as NA", {
  dat <- data.frame(Q1 = c(1, NA), Q2 = c(3, NA))
  plot <- scale_plot(dat, c("Q1", "Q2"), "Missing data")
  expect_equal(plot$data$y.mean, c(2, NA))
})

test_that("theme_norse_bw returns a complete ggplot theme", {
  theme <- theme_norse_bw()
  expect_s3_class(theme, "theme")
  expect_true(isTRUE(attr(theme, "complete")))
  expect_equal(theme$text@colour, "black")
})

test_that("scale_analysis2 print and plot methods return their object invisibly", {
  plot <- ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) + ggplot2::geom_point()
  object <- structure(list(
    scale_name = "Example",
    items = c("Q1", "Q2"),
    item_text = c("One", "Two"),
    min = 1, max = 5, zmin = -1, zmax = 1,
    floor = .1, ceiling = .2, pct_open_scale = .9,
    reliability = data.frame(alpha = .8),
    cor = diag(2), info = data.frame(item = c("Q1", "Q2")),
    tables = list(table(c(1, 2)), table(c(2, 2))),
    histogram = plot, ICC = plot, TIF = plot, IIC = plot
  ), class = "scale_analysis2")

  printed <- capture.output(returned_print <- print(object))
  plotted <- capture.output(returned_plot <- plot(object))

  expect_identical(returned_print, object)
  expect_identical(returned_plot, object)
  expect_true(any(grepl("Scale Name:  Example", printed, fixed = TRUE)))
  expect_length(plotted, 0L)
})
