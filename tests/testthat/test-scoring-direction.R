scoring_direction_fixture <- function(version) {
  metadata <- if (version == "2") nf2.1.item.descriptions else NF3.1_items
  items <- unique(c(metadata$item, "Q226", "Q148", "Q148.2"))
  coded <- as.data.frame(setNames(lapply(items, function(x) c(1, 7, -98, -99, NA_real_)), items))
  coded$Ver_10 <- paste0(version, ".1")
  coded$unrelated <- c(-98, -99, 2, 3, 4)
  agreement <- coded
  positive <- intersect(setdiff(reverse_items(version), nf_scoring_exceptions(version)), items)
  for (item in positive) {
    valid <- agreement[[item]] %in% 1:7
    agreement[[item]][valid] <- 8 - agreement[[item]][valid]
  }
  list(coded = coded, agreement = agreement)
}

test_that("preparation aligns agreement and standardized data for both versions", {
  for (version in c("2", "3")) {
    f <- scoring_direction_fixture(version)
    canonical <- prepare_nf_items(f$coded, "higher_is_worse", version)
    expect_equal(prepare_nf_items(f$agreement, "agreement", version), canonical)
    expect_equal(prepare_nf_items(canonical, "higher_is_worse", version), canonical)
    expect_identical(canonical$unrelated, f$coded$unrelated)
    expect_equal(canonical$Q100, c(1, 7, 1, NA, NA))
    for (item in intersect(nf_scoring_exceptions(version), names(canonical))) {
      expect_equal(canonical[[item]], c(1, 7, NA, NA, NA), info = item)
    }
  }
})

test_that("scorers agree across input conventions while preserving original items", {
  for (version in c("2", "3")) {
    f <- scoring_direction_fixture(version)
    scorer <- if (version == "2") score_all_NORSE2 else score_all_nf3
    a <- score_all(f$agreement, input_coding = "agreement")
    b <- score_all(f$coded, input_coding = "higher_is_worse")
    scores <- setdiff(names(a), names(f$coded))
    expect_equal(a[scores], b[scores])
    expect_equal(a[names(f$agreement)], f$agreement)
    direct <- scorer(f$agreement, input_coding = "agreement")
    common <- intersect(scores, names(direct))
    expect_equal(a[common], direct[common])
    expect_equal(direct[names(f$agreement)], f$agreement)
    expect_equal(a$worry, c(1, 7, 1, NA, NA))
    expect_equal(a$genFunc, c(1, 7, 1, NA, NA))
    expect_equal(a$QOL, c(1, 7, NA, NA, NA))
    expect_equal(a$alliance, c(1, 7, NA, NA, NA))
    expect_equal(a$pref, c(1, 7, NA, NA, NA))
    if (version == "3") {
      expect_equal(a$socSup, c(1, 7, 1, NA, NA))
      expect_equal(a$selfComp, c(1, 7, 1, NA, NA))
    }
  }
})

test_that("NF2 over-under scoring shares preparation and leaves exceptions unchanged", {
  f <- scoring_direction_fixture("2")
  a <- score_all_NORSE2_ou(f$agreement, input_coding = "agreement")
  b <- score_all_NORSE2_ou(f$coded, input_coding = "higher_is_worse")
  scores <- setdiff(names(a), names(f$coded))
  expect_equal(a[scores], b[scores])
  expect_equal(a[names(f$agreement)], f$agreement)
  expect_equal(a$alliance_ou, c(1, 7, NA, NA, NA))
  expect_equal(a$needs_ou, c(1, 7, NA, NA, NA))
})

test_that("batch and item overrides are explicit and never reverse exceptions", {
  f <- scoring_direction_fixture("3")
  dat <- rbind(f$coded, f$agreement)
  coding <- rep(c("higher_is_worse", "agreement"), each = 5)
  out <- score_all(dat, input_coding = coding)
  expect_equal(out$socSup, rep(c(1, 7, 1, NA, NA), 2))
  f$agreement$Q223 <- f$coded$Q223
  out <- score_all(f$agreement, input_coding = "agreement",
                   item_coding = c(Q223 = "higher_is_worse", Q226 = "agreement"))
  expect_equal(out$impulsivity, c(1, 7, 1, NA, NA))
  expect_equal(out$QOL, c(1, 7, NA, NA, NA))
})

test_that("mixed-version preparation matches separate scoring and excludes inactive items", {
  nf2 <- scoring_direction_fixture("2")$agreement
  nf3 <- scoring_direction_fixture("3")$agreement
  dat <- dplyr::bind_rows(nf2, nf3)
  out <- score_all(dat, input_coding = "agreement")
  expect_equal(out$genFunc, rep(c(1, 7, 1, NA, NA), 2))
  expect_equal(out$alliance, rep(c(1, 7, NA, NA, NA), 2))
  excluded <- score_all(dat, versions = "2", input_coding = "agreement")
  expect_true(all(is.na(excluded$QOL[6:10])))
  expect_identical(excluded$Q226, dat$Q226)
})

test_that("low-level aggregators and reversal reject unprepared sentinel codes", {
  dat <- data.frame(Q1 = c(-98, -99), Q2 = c(2, 3))
  expect_error(score_NORSE_trigger(dat), "outside scoring range")
  expect_error(score_NORSE_mean(dat), "outside scoring range")
  expect_error(score_NORSE_overunder(dat), "outside scoring range")
  expect_error(rev_score(c(-98, -99)), "outside scoring range")
  expect_equal(score_NORSE_trigger(data.frame(Q1 = c(1, NA), Q2 = c(NA, NA))), c(1, NA))
})

test_that("preparation validates the coding contract and values", {
  dat <- data.frame(Q223 = c(1, -98))
  expect_error(prepare_nf_items(dat), "input_coding")
  expect_error(prepare_nf_items(dat, "mixed"), "input_coding")
  expect_error(prepare_nf_items(dat, c("agreement", "agreement", "agreement")), "input_coding")
  expect_error(prepare_nf_items(dat, "agreement", version = "4"), "Unsupported")
  expect_error(prepare_nf_items(dat, "agreement", item_coding = c(unknown="agreement")), "item_coding")
  expect_error(prepare_nf_items(data.frame(Q223 = -97), "agreement"), "Invalid item")
  expect_error(prepare_nf_items(data.frame(Q223 = "1"), "agreement"), "Invalid item")
  expect_equal(nrow(prepare_nf_items(dat[FALSE, , drop=FALSE], "agreement")), 0)
})

test_that("audit exceptions neither suggest reversal nor affect the verdict", {
  x <- rep(1:7, 12)
  f <- list(metadata = data.frame(item = c("neg", "pos", "worry", "support"),
    reverse = c("", "R", "", "R"),
    scale_e = c("Mixed", "Mixed", "Worry", "Social Support")),
    dat = data.frame(neg = x, pos = 8 - x, worry = x, support = 8 - x))
  f$metadata <- rbind(f$metadata,
    data.frame(item=c("Q148", "Q226", "Q235"), reverse=c("R", "", ""),
               scale_e=c("Norse", "Quality of Life", "Alliance (Goal)")))
  f$dat$Q148 <- f$dat$Q226 <- f$dat$Q235 <- f$dat$pos
  a <- audit_nf_reverse(f$dat, f$metadata, anchor_scales = "Worry",
    positive_scale_anchors = list("Social Support" = "Worry"), verbose = TRUE)
  exempt <- subset(a$items, item %in% c("Q148", "Q226", "Q235"))
  expect_equal(a$status, "not consistent")
  expect_true(all(exempt$action == "KEEP"))
  expect_true(all(exempt$status == "not assessed"))
  expect_false(any(a$correlations$scale %in% c("Norse", "Quality of Life", "Alliance (Goal)")))
})

test_that("partial sentinel responses use the correct scale denominators", {
  dat <- data.frame(Ver_10 = c("3.1", "3.1"),
    Q205 = 7, Q151 = 7, Q223 = c(-98, -99), Q224 = 7,
    Q236 = 7, Q235 = c(-98, -99), Q237 = NA_real_,
    Q71 = 7, Q74 = c(-98, -99), Q152 = NA_real_, Q153 = NA_real_)
  out <- score_all(dat, input_coding = "agreement")
  expect_equal(out$impulsivity, c(5.5, 7))
  expect_equal(out$alliance, c(7, 7))
  expect_equal(out$pref, c(7, 7))
})
