audit_fixture <- function() {
  x <- rep(1:7, 12)
  list(metadata = data.frame(
    item = c("neg", "pos", "worry", "support"),
    reverse = c("", "R ", "", "R"),
    scale_e = c("Mixed", "Mixed", "Worry", "Social Support")),
    dat = data.frame(neg = x, pos = 8 - x, worry = x, support = 8 - x))
}
run_audit_fixture <- function(dat = NULL, metadata = NULL, ...) {
  f <- audit_fixture()
  if (is.null(dat)) dat <- f$dat
  if (is.null(metadata)) metadata <- f$metadata
  audit_nf_reverse(dat, metadata, anchor_scales = "Worry",
                   positive_scale_anchors = list("Social Support" = "Worry"), ...)
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
  expect_equal(a$classifications$classification[3], "UNCERTAIN")
  expect_true(all(subset(a$correlations, type == "scale_anchor")$n == 0))
  b <- run_audit_fixture(f$dat, f$metadata, min_fraction = 0.5)
  expect_equal(b$classifications$classification[3], "RAW_AGREEMENT")
  f <- audit_fixture()
  f$dat$neg <- 4
  c <- run_audit_fixture(f$dat)
  expect_true(all(is.na(subset(c$correlations, type == "item_rest" & left == "pos")$r)))
})

test_that("single-item scales without anchors have typed empty diagnostics", {
  metadata <- data.frame(item = "x", reverse = "R", scale_e = "Readiness")
  a <- audit_nf_reverse(data.frame(x = 1:7), metadata, anchor_scales = character(),
                        positive_scale_anchors = list())
  expect_equal(nrow(a$correlations), 0)
  expect_equal(a$classifications$classification, "UNCERTAIN")
  expect_equal(a$transformations$action, "UNRESOLVED")
})

test_that("inputs and anchor definitions are validated", {
  f <- audit_fixture()
  expect_error(run_audit_fixture(transform(f$dat, pos = -97)), "numeric 1:7")
  expect_error(run_audit_fixture(transform(f$dat, pos = "1")), "numeric 1:7")
  expect_error(run_audit_fixture(min_n = 3), "min_n")
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
  a <- audit_nf_reverse(dat)
  expect_true(all(c("Q117", "Q100", "Q204") %in% a$mapping$item[a$mapping$present]))
  expect_true(any(a$mapping$positive))
})
