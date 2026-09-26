audit_fixture <- function() {
  x <- rep(1:7, 12)
  list(metadata = data.frame(
    item = c("neg", "pos", "worry", "support"),
    reverse = c("", "R ", "", "R"),
    scale_e = c("Mixed", "Mixed", "Worry", "Social Support")),
    dat = data.frame(neg = x, pos = 8 - x, worry = x, support = 8 - x))
}
run_audit_fixture <- function(dat = NULL, metadata = NULL, verbose = TRUE, ...) {
  f <- audit_fixture()
  if (is.null(dat)) dat <- f$dat
  if (is.null(metadata)) metadata <- f$metadata
  audit_nf_reverse(dat, metadata, anchor_scales = "Worry",
                   positive_scale_anchors = list("Social Support" = "Worry"), verbose = verbose, ...)
}

test_that("raw and already reversed exports yield the expected actions", {
  f <- audit_fixture()
  original <- f$dat
  a <- run_audit_fixture(f$dat)
  expect_identical(f$dat, original)
  expect_equal(a$classifications$classification,
               c("RAW_AGREEMENT", "ANCHOR_DIRECTION_ASSUMED", "RAW_AGREEMENT"))
  expect_equal(a$transformations$action, c("KEEP", "REVERSE", "KEEP", "REVERSE"))
  f$dat$pos <- 8 - f$dat$pos
  f$dat$support <- 8 - f$dat$support
  b <- run_audit_fixture(f$dat)
  expect_true(all(b$transformations$action == "KEEP"))
  expect_equal(b$classifications$classification[c(1, 3)], rep("ALREADY_HIGHER_IS_WORSE", 2))
})

test_that("sentinels are handled before reversal and never enter correlations", {
  f <- audit_fixture()
  f$dat <- rbind(f$dat, c(-98, -98, -98, -98), c(-99, -99, -99, -99), c(NA, NA, NA, NA))
  a <- run_audit_fixture(f$dat)
  z <- subset(a$correlations, scale == "Mixed" & type == "item_pair" & hypothesis == "RAW_AGREEMENT")
  expect_equal(z$n, c(84, 85))
  expect_equal(z$r, c(1, 1))
  cts <- subset(a$counts, scope == "analysis")
  expect_equal(cts$no_problem, rep(1, 4))
  expect_equal(cts$missing_code, rep(1, 4))
  expect_equal(cts$missing, rep(1, 4))
  f$dat$pos[1:84] <- 8 - f$dat$pos[1:84]
  b <- run_audit_fixture(f$dat)
  z <- subset(b$correlations, scale == "Mixed" & type == "item_pair" &
                hypothesis == "ALREADY_HIGHER_IS_WORSE")
  expect_equal(z$r, c(1, 1))
})

test_that("special-value sensitivity cannot supply primary evidence", {
  f <- audit_fixture()
  f$dat$neg <- rep(4:7, 21)
  f$dat$pos <- 11 - f$dat$neg
  extras <- f$dat[rep(1, 1000), ]
  extras$neg <- extras$pos <- -98
  a <- run_audit_fixture(rbind(f$dat, extras))
  mixed <- subset(a$classifications, scale == "Mixed")
  expect_equal(mixed$primary, "RAW_AGREEMENT")
  expect_equal(mixed$classification, "SENSITIVE_TO_NO_PROBLEM_CODES")
  expect_equal(a$transformations$action[a$transformations$item == "pos"], "UNRESOLVED")
  f$dat[,] <- -98
  b <- run_audit_fixture(f$dat)
  expect_true(all(b$classifications$classification[c(1, 3)] == "UNCERTAIN"))
})

test_that("weak, constant, and undersized evidence stays unresolved", {
  a <- run_audit_fixture(audit_fixture()$dat[1:7, ])
  expect_equal(a$classifications$classification[c(1, 3)], rep("UNCERTAIN", 2))
  f <- audit_fixture()
  f$dat$pos <- 4
  b <- run_audit_fixture(f$dat)
  expect_equal(b$classifications$classification[1], "UNCERTAIN")
  expect_true(all(is.na(subset(b$correlations, scale == "Mixed")$r)))
})

test_that("default threshold detects modest but precise directional correlations", {
  set.seed(2026)
  n <- 800
  negative_latent <- stats::rnorm(n)
  positive_latent <- -0.18 * negative_latent +
    sqrt(1 - 0.18^2) * stats::rnorm(n)
  ordinal <- function(x) as.integer(cut(
    x, breaks = stats::quantile(x, probs = 0:7 / 7),
    include.lowest = TRUE, labels = FALSE
  ))
  dat <- data.frame(negative = ordinal(negative_latent),
                    positive = ordinal(positive_latent))
  metadata <- data.frame(item = c("negative", "positive"),
                         reverse = c("", "R"), scale_e = "Mixed")

  default <- audit_nf_reverse(
    dat, metadata, anchor_scales = character(),
    positive_scale_anchors = list(), verbose = TRUE
  )
  stricter <- audit_nf_reverse(
    dat, metadata, anchor_scales = character(),
    positive_scale_anchors = list(), min_abs_r = 0.20, verbose = TRUE
  )

  expect_equal(default$settings$min_abs_r, 0.15)
  expect_equal(default$classifications$classification, "RAW_AGREEMENT")
  expect_equal(stricter$classifications$classification, "UNCERTAIN")
})

test_that("partial reversal and conflicting items remain visible", {
  f <- audit_fixture()
  f$dat$support <- 8 - f$dat$support
  a <- run_audit_fixture(f$dat)
  expect_true(a$groups$possible_partial_reversal)
  f$metadata <- rbind(f$metadata, data.frame(item = "pos2", reverse = "R", scale_e = "Mixed"))
  f$dat$pos2 <- 8 - f$dat$pos
  b <- run_audit_fixture(f$dat, f$metadata)
  expect_equal(b$classifications$classification[1], "INCONSISTENT")
  expect_true(all(b$transformations$action[b$transformations$positive &
                                           b$transformations$scale == "Mixed"] == "UNRESOLVED"))
})

test_that("batches and repeated patients are analyzed separately", {
  f <- audit_fixture()
  raw <- f$dat
  coded <- transform(raw, pos = 8 - pos, support = 8 - support)
  dat <- rbind(raw, coded)
  dat$batch <- rep(c("raw", "coded"), each = nrow(raw))
  dat$patient <- rep(seq_len(nrow(raw)), 2)
  dat$time <- 1
  later <- transform(dat, time = 2, pos = 4)
  a <- run_audit_fixture(rbind(later, dat), group_vars = "batch",
                         patient_id = "patient", order_by = "time")
  expect_equal(a$groups$n_export, c(168L, 168L))
  expect_equal(a$groups$n_analysis, c(84L, 84L))
  expect_equal(subset(a$classifications, scale == "Mixed")$classification,
               c("RAW_AGREEMENT", "ALREADY_HIGHER_IS_WORSE"))
  expect_true(a$settings$differing_conventions_across_groups)
  dat$a <- rep(c("a.b", "a"), each = 84)
  dat$b <- rep(c("c", "b.c"), each = 84)
  expect_equal(nrow(run_audit_fixture(dat, group_vars = c("a", "b"))$groups), 2)
})

test_that("mapping preserves version suffixes and reports absent items", {
  f <- audit_fixture()
  f$metadata$item[2] <- "Q140.1"
  a <- run_audit_fixture(f$dat, f$metadata, item_map = c(Q140.1 = "pos"))
  expect_true(all(a$mapping$present))
  expect_equal(a$classifications$classification[1], "RAW_AGREEMENT")
  b <- run_audit_fixture(f$dat, f$metadata)
  expect_false(b$mapping$present[2])
  expect_true("pos" %in% b$unmapped_columns)
  expect_equal(b$classifications$classification[1], "UNCERTAIN")
  csv <- data.frame("CODE (prefixed with Q)" = c("1", "2.1", "3", "4"),
                    "Reverse score" = f$metadata$reverse,
                    "English: dimension/subscale" = f$metadata$scale_e,
                    check.names = FALSE)
  c <- run_audit_fixture(f$dat, csv, item_map = c(Q1="neg", Q2.1="pos", Q3="worry", Q4="support"))
  expect_equal(c$mapping$item, c("Q1", "Q2.1", "Q3", "Q4"))
})

test_that("completeness counts absent metadata items and item-rest excludes target", {
  f <- audit_fixture()
  f$metadata <- rbind(f$metadata, data.frame(item = "absent", reverse = "R", scale_e = "Social Support"))
  a <- run_audit_fixture(f$dat, f$metadata)
  expect_equal(a$classifications$classification[3], "RAW_AGREEMENT")
  strict <- run_audit_fixture(f$dat, f$metadata, min_items = 2)
  expect_equal(strict$classifications$classification[3], "UNCERTAIN")
  expect_true(all(subset(strict$correlations, type == "scale_anchor")$n == 0))
  proportional <- run_audit_fixture(f$dat, f$metadata, min_fraction = 0.5)
  expect_equal(proportional$classifications$classification[3], "RAW_AGREEMENT")
  f <- audit_fixture()
  f$dat$neg <- 4
  c <- run_audit_fixture(f$dat)
  expect_true(all(is.na(subset(c$correlations, type == "item_rest" & left == "pos")$r)))
})

test_that("single-item scales without anchors have typed empty diagnostics", {
  metadata <- data.frame(item = "x", reverse = "R", scale_e = "Readiness")
  a <- audit_nf_reverse(data.frame(x = 1:7), metadata, anchor_scales = character(),
                        positive_scale_anchors = list(), verbose = TRUE)
  expect_equal(nrow(a$correlations), 0)
  expect_equal(a$classifications$classification, "UNCERTAIN")
  expect_equal(a$transformations$action, "UNRESOLVED")
})

test_that("inputs and anchor definitions are validated", {
  f <- audit_fixture()
  expect_error(run_audit_fixture(transform(f$dat, pos = -97)), "numeric 1:7")
  expect_error(run_audit_fixture(transform(f$dat, pos = "1")), "numeric 1:7")
  expect_error(run_audit_fixture(min_n = 3), "min_n")
  expect_error(run_audit_fixture(min_items = 0), "positive integer")
  expect_error(run_audit_fixture(min_items = 1.5), "positive integer")
  expect_error(run_audit_fixture(min_fraction = 0), "Invalid")
  expect_error(run_audit_fixture(item_map = c(pos = "neg")), "one-to-one")
  expect_error(run_audit_fixture(item_map = c(unknown = "x")), "known metadata")
  expect_error(run_audit_fixture(patient_id = "missing"), "existing columns")
  expect_error(run_audit_fixture(order_by = "neg"), "requires patient_id")
  expect_error(run_audit_fixture(patient_id = "neg"), "Control columns")
  expect_error(run_audit_fixture(transform(f$dat, batch = NA), group_vars = "batch"), "missing values")
  bad <- f$metadata
  bad$reverse[1] <- "maybe"
  expect_error(run_audit_fixture(metadata = bad), "blank or R")
  expect_error(audit_nf_reverse(f$dat, f$metadata, anchor_scales = "Mixed"), "negative-only")
  expect_error(audit_nf_reverse(f$dat, f$metadata, anchor_scales = "Worry",
                                positive_scale_anchors = list(Mixed = "Worry")), "entirely positive")
})

test_that("default bundled metadata is usable without changing data", {
  dat <- data.frame(Q117 = rep(1:7, 12), Q100 = rep(1:7, 12), Q204 = rep(7:1, 12))
  a <- audit_nf_reverse(dat, verbose = TRUE)
  expect_true(all(c("Q117", "Q100", "Q204") %in% a$mapping$item[a$mapping$present]))
  expect_true(any(a$mapping$positive))
})

test_that("QOL uses its 0 to 10 response range and is not audited for reversal", {
  qol <- c(NA, 6, 7, 4, 5, 3, 1, 2, 8, 0, 10, 9, -98, -99)
  a <- expect_no_warning(audit_nf_reverse(data.frame(Q226 = qol), verbose = TRUE))
  item <- subset(a$items, item == "Q226")
  counts <- subset(a$counts, item == "Q226" & scope == "analysis")

  expect_equal(item$status, "not assessed")
  expect_equal(item$action, "KEEP")
  expect_equal(counts$ordinary, 11)
  expect_equal(counts$no_problem, 1)
  expect_equal(counts$missing_code, 1)
  expect_error(audit_nf_reverse(data.frame(Q226 = 11)), "numeric 0:10")
})

test_that("the default is one plain dataset verdict", {
  f <- audit_fixture()
  raw <- run_audit_fixture(verbose = FALSE)
  expect_identical(raw, "not consistent")
  expect_null(attributes(raw))
  coded <- transform(f$dat, pos = 8 - pos, support = 8 - support)
  expect_identical(run_audit_fixture(coded, verbose = FALSE), "consistent")
  partial <- transform(f$dat, support = 8 - support)
  expect_identical(run_audit_fixture(partial, verbose = FALSE), "mixed")
  expect_identical(run_audit_fixture(coded)$status,
                   run_audit_fixture(coded, verbose = FALSE))
  expect_error(run_audit_fixture(verbose = NA), "single non-missing logical")
  expect_error(run_audit_fixture(verbose = 1), "single non-missing logical")
  expect_error(run_audit_fixture(verbose = c(TRUE, FALSE)), "single non-missing logical")
})

test_that("verbose results make per-item interpretation accessible", {
  a <- run_audit_fixture()
  expect_identical(a$status, "not consistent")
  expect_equal(nrow(a$issues), 0)
  expect_equal(a$items$status, c("assumed consistent", "not consistent",
                                "assumed consistent", "not consistent"))
  expect_equal(a$groups$status, "not consistent")
  expect_equal(a$items$action, a$transformations$action)
})

test_that("dataset verdict combines groups without hiding unresolved evidence", {
  f <- audit_fixture()
  coded <- transform(f$dat, pos = 8 - pos, support = 8 - support)
  dat <- rbind(transform(f$dat, batch = "raw"), transform(coded, batch = "coded"))
  expect_identical(run_audit_fixture(dat, group_vars = "batch", verbose = FALSE), "mixed")
  expect_equal(run_audit_fixture(dat, group_vars = "batch")$groups$status,
               c("not consistent", "consistent"))
  weak <- transform(coded, pos = 4, support = 4, batch = "weak")
  dat <- rbind(transform(coded, batch = "coded"), weak)
  expect_warning(status <- run_audit_fixture(dat, group_vars = "batch", verbose = FALSE),
                 "Insufficient or unresolved evidence")
  expect_identical(status, NA_character_)
  expect_no_warning(details <- run_audit_fixture(dat, group_vars = "batch"))
  expect_identical(details$status, NA_character_)
  expect_true(nrow(details$issues) >= 1)
  expect_true(all(c("group", "scale", "classification", "issue") %in%
                    names(details$issues)))
  expect_true(any(grepl("no variation", details$issues$issue)))
})

test_that("uncertainty is not labelled mixed or consistent", {
  f <- audit_fixture()
  for (dat in list(f$dat[1:7, ], transform(f$dat, pos = 4),
                   as.data.frame(lapply(f$dat, function(x) rep(-98, length(x)))))) {
    expect_warning(status <- run_audit_fixture(dat, verbose = FALSE), "verbose = TRUE")
    expect_identical(status, NA_character_)
  }
  f$dat$neg <- rep(4:7, 21)
  f$dat$pos <- 11 - f$dat$neg
  extras <- f$dat[rep(1, 1000), ]
  extras$neg <- extras$pos <- -98
  expect_warning(status <- run_audit_fixture(rbind(f$dat, extras), verbose = FALSE),
                 "verbose = TRUE")
  expect_identical(status, NA_character_)
})

test_that("NA warnings explain the specific audit limitation", {
  f <- audit_fixture()
  expect_warning(
    status <- run_audit_fixture(f$dat[1:7, ], verbose = FALSE),
    "Too few complete response pairs: maximum n = 7; min_n = 30"
  )
  expect_identical(status, NA_character_)

  f$dat$pos <- 4
  expect_warning(
    status <- run_audit_fixture(f$dat, verbose = FALSE),
    "Mixed: The compared responses had no variation"
  )
  expect_identical(status, NA_character_)

  details <- run_audit_fixture(f$dat)
  mixed_issue <- subset(details$issues, scale == "Mixed")
  expect_equal(mixed_issue$classification, "UNCERTAIN")
  expect_match(mixed_issue$issue, "no variation")
})

test_that("within-scale conflict produces a mixed dataset verdict", {
  f <- audit_fixture()
  f$metadata <- rbind(f$metadata, data.frame(item = "pos2", reverse = "R", scale_e = "Mixed"))
  f$dat$pos2 <- 8 - f$dat$pos
  expect_identical(run_audit_fixture(f$dat, f$metadata, verbose = FALSE), "mixed")
  expect_true(all(subset(run_audit_fixture(f$dat, f$metadata)$items,
                         scale == "Mixed")$status == "mixed"))
  # A conflict in a negative-only scale also prevents a consistent verdict.
  f <- audit_fixture()
  f$metadata <- rbind(f$metadata, data.frame(item = "worry2", reverse = "", scale_e = "Worry"))
  f$dat <- transform(f$dat, pos = 8 - pos, support = 8 - support, worry2 = 8 - worry)
  expect_identical(run_audit_fixture(f$dat, f$metadata, verbose = FALSE), "mixed")
})

test_that("unassessed scales do not silently count as consistent votes", {
  f <- audit_fixture()
  f$metadata <- rbind(f$metadata,
    data.frame(item = "readiness", reverse = "R", scale_e = "Readiness"),
    data.frame(item = "absent", reverse = "R", scale_e = "Absent"))
  f$dat$readiness <- rep(1:7, 12)
  a <- run_audit_fixture(f$dat, f$metadata)
  expect_equal(a$status, "not consistent")
  expect_equal(tail(a$items$status, 2), rep("not assessed", 2))
  only_negative <- data.frame(item = "x", reverse = "", scale_e = "Worry")
  expect_warning(status <- audit_nf_reverse(data.frame(x = rep(1:7, 12)),
    only_negative, anchor_scales = "Worry", positive_scale_anchors = list()), "verbose = TRUE")
  expect_identical(status, NA_character_)
})
