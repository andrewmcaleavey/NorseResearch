#' Audit the direction of exported Norse Feedback item responses
#'
#' Compare original agreement coding with coding in which higher values always
#' indicate more problems. By default, return a single dataset-level verdict.
#' Use `verbose = TRUE` for item-level results and supporting evidence. This
#' diagnostic does not modify or score `dat`.
#'
#' @param dat A data frame of exported item responses. Item columns must be
#'   numeric and contain their ordinary response values, -98, -99, or `NA`.
#'   Most NF items use 1--7; QOL item Q226 uses 0--10.
#' @param metadata A data frame with columns `item` (unique item identifier),
#'   `reverse` (logical, or blank/`R` character flags), and `scale_e` (scale name).
#'   Defaults to the bundled NF3.1 metadata. The original NF3.1 CSV column names
#'   are also accepted when read with `check.names = FALSE`; its item codes are
#'   prefixed with `Q` and version suffixes are preserved. Surrounding whitespace
#'   in reverse flags is ignored. A reverse flag identifies positive wording,
#'   not proof that exported responses have already been reversed.
#' @param item_map Optional named character vector mapping metadata item IDs
#'   (names) to exported column names (values). Unspecified IDs map to themselves.
#'   Mappings must be one-to-one. No suffixes are removed automatically.
#' @param group_vars Optional character vector of columns defining independent
#'   export groups, such as source, software version, or batch. Missing group
#'   values are not allowed. With `NULL`, all rows form one group.
#' @param patient_id Optional patient identifier column. Within each export
#'   group only the first row per patient is used, after sorting by `order_by`
#'   if supplied. Without an identifier, rows are assumed independent.
#' @param order_by Optional column used to order assessments before selecting
#'   one per patient; ties retain input order. Requires `patient_id`. Missing
#'   identifiers or ordering values are not allowed.
#' @param anchor_scales Names of negative-only scales whose exported direction
#'   is trusted to be higher = worse. Defaults to Worry and Sad Affect. Unknown
#'   names and scales containing positive items are errors.
#' @param positive_scale_anchors Named list mapping entirely positive scale
#'   names to relevant `anchor_scales`. Defaults to Social Support against Worry
#'   and Sad Affect. Only specified relationships are used: after conversion to
#'   higher = worse, each is expected to be positive. Use `list()` to disable.
#' @param min_n Minimum number of complete pairs for a correlation to provide
#'   directional evidence. An integer of at least 4; default 30.
#' @param min_abs_r Minimum absolute Pearson correlation providing directional
#'   evidence; default 0.15. Must be greater than 0 and less than 1.
#' @param conf_level Confidence level for approximate Fisher-z correlation
#'   intervals; default 0.95. Must be greater than 0 and less than 1.
#' @param min_items Minimum number of observed items needed to calculate a
#'   scale or item-rest mean; default 1. For item-rest means this means one
#'   observed item other than the target item.
#' @param min_fraction Optional minimum proportion of a scale's metadata items
#'   needed to calculate its mean. The default `NULL` imposes no proportional
#'   requirement, so `min_items = 1` controls the default. When supplied, it
#'   must be greater than 0 and at most 1. `min_items` and `min_fraction` are
#'   both enforced; absent export columns count as missing items.
#'
#' @param verbose A single logical value, default `FALSE`. Return a detailed
#'   audit list instead of the single overall verdict when `TRUE`.
#'
#' @details
#' The default verdict is relative to higher = worse coding:
#' `"consistent"` means all assessable positive-item scales/groups support
#' already reversed responses; `"not consistent"` means they all support
#' original agreement responses; `"mixed"` means supported conventions differ
#' across scales/groups, a scale has conflicting directional evidence, or a
#' supported scale has an item conflict. Negative items are not counted as a
#' second convention when positive items retain original agreement coding.
#'
#' An assessment includes mixed-wording scales and entirely positive scales
#' listed in `positive_scale_anchors`, provided the group's analysis sample has
#' ordinary responses on at least one positive item in that scale. Absent or
#' all-missing/sentinel-only scales and unconfigured positive scales do not vote.
#' The verdict describes the available diagnostic evidence, not verification of
#' every exported item. Items outside the assessment are marked `"not assessed"`
#' in the verbose output. Unknown export columns are not assessed either.
#'
#' If any included scale is unresolved, or none can be assessed, the default
#' returns `NA_character_` with a warning to request verbose details. Established
#' mixed coding takes precedence over unresolved evidence. An unassessable
#' export group prevents an otherwise uniform dataset verdict. Verbose output
#' records these limitations without warning. Weak evidence is not called mixed.
#'
#' Two hypotheses are evaluated: `RAW_AGREEMENT` reverses ordinary responses
#' on positive items with `8 - x`; `ALREADY_HIGHER_IS_WORSE` leaves ordinary
#' responses unchanged. Both candidates therefore have higher = worse coding.
#' Under both hypotheses, -99 becomes missing and -98 becomes 1 in the candidate
#' dataset, except for Alliance, Therapy Preferences/Needs, QOL, and Norse,
#' where both codes are missing. These items retain their exported direction
#' and are excluded from the problem-direction audit, as in [prepare_nf_items()].
#' In original agreement units, -98 would instead be 7 on a positive
#' item; converting that 7 to higher = worse also gives 1.
#'
#' Each hypothesis is checked first excluding -98 and then including its
#' recoded value. Exclusion is a diagnostic sensitivity analysis, not a proposed
#' final scoring rule. Original `NA` and -99 are always missing. Already recoded
#' special values cannot be identified by this function.
#'
#' All within-scale item pairs and corrected item-rest correlations are
#' reported. For mixed-wording scales, only opposite-worded item pairs decide
#' direction. Entirely positive scales use only their specified independent
#' symptom anchors. Internal correlations cannot identify the direction of an
#' entirely positive scale. Single-item scales can be checked against anchors.
#'
#' A correlation provides directional evidence only if its pair count is at
#' least `min_n`, its magnitude is at least `min_abs_r`, and its interval excludes
#' zero. A candidate is supported when there is at least one positive piece of
#' deciding evidence and no negative piece. Exactly one supported candidate is
#' required. Conflicting evidence returns `INCONSISTENT`; weak or unavailable
#' evidence returns `UNCERTAIN`. If the primary decision is resolved but changes
#' when -98 is included, the final decision is `SENSITIVE_TO_NO_PROBLEM_CODES`.
#' Sensitivity evidence never upgrades an unresolved primary decision.
#'
#' Fisher-z intervals assume independent observations and are approximate,
#' particularly for ordinal, restricted-range responses. They are diagnostic
#' intervals, without multiplicity adjustment, not formal proof of export
#' coding. Exact correlations of -1 or 1 receive degenerate intervals. Thresholds
#' are configurable diagnostic defaults, not validated NF cutoffs. Correlations
#' cannot establish absolute direction without trusted anchors, distinguish all
#' response-process effects from coding errors, or diagnose arbitrary row-level
#' mixtures. Negative-only items are assumed to retain higher = worse coding.
#'
#' The proposed transformation map refers only to ordinary 1--7 responses.
#' Negative items are `KEEP` under the trusted-direction assumption. Positive
#' items are `REVERSE` for raw agreement, `KEEP` for already reversed exports,
#' and `UNRESOLVED` otherwise. Even resolved decisions require review: an item
#' can disagree with the rest of its scale. No automatic item-by-item search for
#' the highest reliability is performed. When later applying a reviewed map,
#' use [prepare_nf_items()] to handle special codes separately from reversal.
#' The named scoring exceptions always have action `KEEP` and item status
#' `"not assessed"`; they do not contribute to the dataset verdict.
#'
#' @return With `verbose = FALSE`, a character scalar: `"consistent"`,
#'   `"not consistent"`, or `"mixed"`; `NA_character_` when the evidence cannot
#'   establish a verdict. With `verbose = TRUE`, a list containing:
#' \describe{
#'   \item{status}{The same dataset-level verdict as the default return value.}
#'   \item{issues}{A short table explaining each unresolved group or scale that
#'     prevents a dataset verdict. It distinguishes unavailable comparisons,
#'     too few complete pairs, constant responses, weak correlations,
#'     confidence intervals crossing zero, and dependence on -98 recoding.}
#'   \item{items}{Per-group, per-item results: scale, item and export column,
#'     positive wording, presence, proposed ordinary-response action,
#'     item-conflict flag, scale classification, and status (`"consistent"`,
#'     `"not consistent"`, `"mixed"`, `"unresolved"`, `"assumed consistent"`,
#'     or `"not assessed"`). Item direction is inherited from the scale's
#'     evidence; it is not an independently established encoding for every item.}
#' }
#'   The detailed list also retains these diagnostic components:
#' \describe{
#'   \item{classifications}{One row per group and metadata scale, including
#'     scale type, primary and sensitivity decisions, and final classification.}
#'   \item{correlations}{One row per diagnostic comparison, hypothesis, and
#'     special-value treatment. Includes group, scale, comparison type, item or
#'     anchor labels, whether it decides direction, complete-pair count `n`,
#'     `r`, `lower`, `upper`, and evidence (`positive`, `negative`, or `weak`).
#'     Constant or insufficient data yield missing correlations or intervals.}
#'   \item{counts}{Per-group, per-item counts before patient selection and in
#'     the selected analysis sample: ordinary responses, -98, -99, and `NA`.}
#'   \item{transformations}{Per-group, per-item proposed ordinary-response
#'     action, export-column presence, and a flag for negative pair or item-rest
#'     evidence under the selected hypothesis in either special-value analysis.}
#'   \item{groups}{Group identifiers and defining values, row and patient/sample
#'     counts, a per-group `status`, and possible partial reversal within a group.}
#'   \item{mapping}{Item metadata, mapped export columns, and presence flags.}
#'   \item{unmapped_columns}{Export columns not mapped to an item or declared
#'     grouping, patient, or ordering variable. These may include other metadata.}
#'   \item{settings}{Arguments governing interpretation and analysis, and a flag
#'     for differing resolved conventions across export groups.}
#' }
#' @examples
#' metadata <- data.frame(
#'   item = c("problem", "strength", "worry", "support"),
#'   reverse = c("", "R", "", "R"),
#'   scale_e = c("Mixed", "Mixed", "Worry", "Social Support")
#' )
#' x <- rep(1:7, 10)
#' dat <- data.frame(problem = x, strength = 8 - x,
#'                   worry = x, support = 8 - x)
#' audit <- audit_nf_reverse(
#'   dat, metadata, anchor_scales = "Worry",
#'   positive_scale_anchors = list("Social Support" = "Worry")
#' )
#' audit # "not consistent": positive items retain agreement coding
#' details <- audit_nf_reverse(
#'   dat, metadata, anchor_scales = "Worry",
#'   positive_scale_anchors = list("Social Support" = "Worry"), verbose = TRUE
#' )
#' details$status
#' details$items
#' # An untouched NF3.1 CSV can also be supplied explicitly:
#' # metadata <- read.csv("NF3.1_items.csv", check.names = FALSE,
#' #                      colClasses = "character")
#' # audit_nf_reverse(dat, metadata, item_map = c(Q140.1 = "Q140"))
#' @export
audit_nf_reverse <- function(dat, metadata = NF3.1_items, item_map = NULL,
                              group_vars = NULL, patient_id = NULL,
                              order_by = NULL,
                              anchor_scales = c("Worry", "Sad Affect"),
                              positive_scale_anchors = list(
                                "Social Support" = c("Worry", "Sad Affect")),
                              min_n = 30L, min_abs_r = 0.15,
                              conf_level = 0.95, min_items = 1L,
                              min_fraction = NULL,
                              verbose = FALSE) {
  fail <- function(message) stop(message, call. = FALSE)
  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose))
    fail("verbose must be a single non-missing logical value.")
  if (!is.data.frame(dat) || !nrow(dat) || anyDuplicated(names(dat)))
    fail("dat must be a nonempty data frame with unique column names.")
  if (!is.data.frame(metadata)) fail("metadata must be a data frame.")
  csv_names <- c("CODE (prefixed with Q)", "Reverse score",
                 "English: dimension/subscale")
  if (all(csv_names %in% names(metadata))) {
    metadata <- data.frame(item = paste0("Q", trimws(as.character(metadata[[csv_names[1]]]))),
                           reverse = metadata[[csv_names[2]]],
                           scale_e = metadata[[csv_names[3]]])
  }
  if (!all(c("item", "reverse", "scale_e") %in% names(metadata)))
    fail("metadata must contain item, reverse, and scale_e columns.")
  m <- metadata[, c("item", "reverse", "scale_e")]
  m$item <- trimws(as.character(m$item))
  m$scale_e <- trimws(as.character(m$scale_e))
  if (!nrow(m) || anyNA(m$item) || any(!nzchar(m$item)) ||
      anyDuplicated(m$item) || anyNA(m$scale_e) || any(!nzchar(m$scale_e)))
    fail("metadata requires unique, nonmissing item IDs and nonempty scale names.")
  if (is.logical(m$reverse)) {
    if (anyNA(m$reverse)) fail("Logical reverse flags cannot be missing.")
    m$positive <- m$reverse
  } else {
    flags <- toupper(trimws(as.character(m$reverse)))
    flags[is.na(flags)] <- ""
    if (any(!flags %in% c("", "R"))) fail("Character reverse flags must be blank or R.")
    m$positive <- flags == "R"
  }
  m$scoring_exception <- m$item %in% nf_scoring_exceptions(c("2", "3"))
  m$column <- m$item
  if (!is.null(item_map)) {
    if (!is.character(item_map) || is.null(names(item_map)) ||
        anyNA(item_map) || any(!nzchar(item_map)) || anyNA(names(item_map)) ||
        anyDuplicated(names(item_map)) || any(!names(item_map) %in% m$item))
      fail("item_map must be a named character vector of known metadata IDs.")
    m$column[match(names(item_map), m$item)] <- unname(item_map)
  }
  if (anyDuplicated(m$column)) fail("Item-to-column mappings must be one-to-one.")
  m$present <- m$column %in% names(dat)
  if (!any(m$present)) fail("No metadata items match exported columns; check item_map.")
  for (j in which(m$present)) {
    v <- m$column[j]
    item <- m$item[j]
    x <- dat[[v]]
    ordinary_values <- nf_item_response_values(item)
    if (!is.numeric(x) || !is.null(dim(x)) ||
        any(!is.na(x) & !x %in% c(ordinary_values, -98, -99))) {
      expected <- if (identical(item, "Q226")) "0:10" else "1:7"
      fail(paste0("Item column ", v, " must contain only numeric ", expected,
                  ", -98, -99, or NA."))
    }
  }
  for (arg in c("group_vars", "patient_id", "order_by")) {
    v <- get(arg)
    if (!is.null(v) && (!is.character(v) || !length(v) || anyNA(v) ||
        anyDuplicated(v) || any(!v %in% names(dat))))
      fail(paste0(arg, " must name existing columns."))
  }
  if (length(patient_id) > 1L || length(order_by) > 1L ||
      (!is.null(order_by) && is.null(patient_id)))
    fail("patient_id and order_by must be single columns; order_by requires patient_id.")
  controls <- unique(c(group_vars, patient_id, order_by))
  if (any(controls %in% m$column)) fail("Control columns cannot also be item columns.")
  if (any(vapply(dat[controls], anyNA, logical(1))))
    fail("Grouping, patient, and ordering columns cannot contain missing values.")
  scalar <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x)
  if (!scalar(min_n) || min_n < 4 || min_n != floor(min_n))
    fail("min_n must be an integer of at least 4.")
  if (!scalar(min_items) || min_items < 1 || min_items != floor(min_items))
    fail("min_items must be a positive integer.")
  if (!scalar(min_abs_r) || min_abs_r <= 0 || min_abs_r >= 1 ||
      !scalar(conf_level) || conf_level <= 0 || conf_level >= 1 ||
      (!is.null(min_fraction) &&
       (!scalar(min_fraction) || min_fraction <= 0 || min_fraction > 1)))
    fail("Invalid min_abs_r, conf_level, or min_fraction.")
  scales <- unique(m$scale_e)
  indices <- lapply(scales, function(s) which(m$scale_e == s))
  names(indices) <- scales
  kinds <- vapply(indices, function(ii) {
    ii <- ii[!m$scoring_exception[ii]]
    if (!length(ii)) return("exception")
    if (all(m$positive[ii])) "positive" else if (any(m$positive[ii])) "mixed" else "negative"
  }, character(1))
  if (!is.character(anchor_scales) || anyNA(anchor_scales) ||
      anyDuplicated(anchor_scales) || any(!anchor_scales %in% scales) ||
      any(kinds[anchor_scales] != "negative"))
    fail("anchor_scales must name known negative-only scales.")
  if (!is.list(positive_scale_anchors) || (length(positive_scale_anchors) &&
      (is.null(names(positive_scale_anchors)) || anyNA(names(positive_scale_anchors)) ||
       anyDuplicated(names(positive_scale_anchors)) ||
       any(!names(positive_scale_anchors) %in% scales) ||
       any(kinds[names(positive_scale_anchors)] != "positive"))))
    fail("positive_scale_anchors must be a named list of entirely positive scales.")
  for (a in positive_scale_anchors) {
    if (!is.character(a) || !length(a) || anyNA(a) || anyDuplicated(a) ||
        any(!a %in% anchor_scales))
      fail("Each positive scale must reference known anchor_scales.")
  }
  # Integer factor levels avoid collisions in group values containing separators.
  if (length(group_vars)) {
    factors <- lapply(dat[group_vars], function(x) factor(match(x, unique(x))))
    key <- do.call(interaction, c(factors, list(drop = TRUE, lex.order = TRUE)))
    group_id <- match(key, unique(key))
  } else group_id <- rep(1L, nrow(dat))
  groups <- data.frame(group = seq_len(max(group_id)))
  if (length(group_vars)) {
    # Keep the user's group-variable names in a separate nested data frame.
    group_values <- dat[match(groups$group, group_id), group_vars, drop = FALSE]
    rownames(group_values) <- NULL
  } else group_values <- data.frame(row.names = seq_len(nrow(groups)))
  groups$values <- I(lapply(seq_len(nrow(groups)), function(i) group_values[i, , drop = FALSE]))
  groups$n_export <- tabulate(group_id)
  groups$n_analysis <- integer(nrow(groups))
  groups$possible_partial_reversal <- FALSE
  correlations <- classifications <- counts <- transformations <- list()
  append_row <- function(x, row) { x[[length(x) + 1L]] <- row; x }
  mean_score <- function(x, ii) {
    z <- x[, ii, drop = FALSE]
    n <- rowSums(!is.na(z))
    ans <- rowMeans(z, na.rm = TRUE)
    fraction_items <- if (is.null(min_fraction)) 1L else
      ceiling(length(ii) * min_fraction)
    required_items <- max(min_items, fraction_items)
    ans[n < required_items] <- NA_real_
    ans
  }
  correlate <- function(x, y) {
    ok <- is.finite(x) & is.finite(y)
    n <- sum(ok)
    r <- lower <- upper <- NA_real_
    if (n >= 3L && stats::sd(x[ok]) > 0 && stats::sd(y[ok]) > 0) {
      r <- max(-1, min(1, stats::cor(x[ok], y[ok])))
      if (n > 3L) {
        delta <- stats::qnorm((1 + conf_level) / 2) / sqrt(n - 3)
        lower <- tanh(atanh(r) - delta)
        upper <- tanh(atanh(r) + delta)
      }
    }
    evidence <- "weak"
    if (n >= min_n && is.finite(lower) && abs(r) >= min_abs_r) {
      if (lower > 0) evidence <- "positive"
      if (upper < 0) evidence <- "negative"
    }
    data.frame(n = n, r = r, lower = lower, upper = upper, evidence = evidence)
  }
  hypotheses <- c("RAW_AGREEMENT", "ALREADY_HIGHER_IS_WORSE")
  decide <- function(z) {
    z <- z[z$deciding, , drop = FALSE]
    support <- vapply(hypotheses, function(h) {
      e <- z$evidence[z$hypothesis == h]
      any(e == "positive") && !any(e == "negative")
    }, logical(1))
    if (sum(support) == 1L) return(hypotheses[support])
    conflict <- any(vapply(hypotheses, function(h) {
      e <- z$evidence[z$hypothesis == h]
      any(e == "positive") && any(e == "negative")
    }, logical(1)))
    if (conflict) "INCONSISTENT" else "UNCERTAIN"
  }
  for (g in groups$group) {
    rows <- which(group_id == g)
    selected <- rows
    if (!is.null(order_by)) selected <- selected[order(dat[[order_by]][selected], selected)]
    if (!is.null(patient_id)) selected <- selected[!duplicated(dat[[patient_id]][selected])]
    groups$n_analysis[g] <- length(selected)
    for (scope in c("export", "analysis")) {
      rr <- if (scope == "export") rows else selected
      for (j in seq_len(nrow(m))) {
        x <- if (m$present[j]) dat[[m$column[j]]][rr] else rep(NA_real_, length(rr))
        counts <- append_row(counts, data.frame(group = g, item = m$item[j],
          scope = scope, present = m$present[j], n = length(x),
          ordinary = sum(x %in% nf_item_response_values(m$item[j])),
          no_problem = sum(x == -98, na.rm = TRUE),
          missing_code = sum(x == -99, na.rm = TRUE), missing = sum(is.na(x))))
      }
    }
    raw <- matrix(NA_real_, length(selected), nrow(m), dimnames = list(NULL, m$item))
    for (j in which(m$present)) raw[, j] <- dat[[m$column[j]]][selected]
    local <- list()
    for (h in hypotheses) for (include98 in c(FALSE, TRUE)) {
      x <- raw
      x[!is.na(x) & x == -99] <- NA_real_
      sentinel <- !is.na(x) & x == -98
      x[sentinel] <- NA_real_
      reverse <- m$positive & !m$scoring_exception
      if (h == "RAW_AGREEMENT") x[, reverse] <- 8 - x[, reverse, drop = FALSE]
      sentinel[, m$scoring_exception] <- FALSE
      if (include98) x[sentinel] <- 1
      add <- function(s, type, left, right, deciding, a, b) {
        local[[length(local) + 1L]] <<- cbind(data.frame(
          group = g, scale = s, hypothesis = h, include_98 = include98,
          type = type, left = left, right = right, deciding = deciding), correlate(a, b))
      }
      for (s in scales) {
        ii <- indices[[s]]
        ii <- ii[!m$scoring_exception[ii]]
        if (!length(ii)) next
        if (length(ii) > 1L) {
          pairs <- utils::combn(ii, 2)
          for (k in seq_len(ncol(pairs))) {
            a <- pairs[1, k]; b <- pairs[2, k]
            add(s, "item_pair", m$item[a], m$item[b], m$positive[a] != m$positive[b],
                x[, a], x[, b])
          }
          for (j in ii) add(s, "item_rest", m$item[j], "rest", FALSE,
                             x[, j], mean_score(x, setdiff(ii, j)))
        }
        if (s %in% names(positive_scale_anchors)) {
          for (a in positive_scale_anchors[[s]]) add(s, "scale_anchor", s, a, TRUE,
                                                    mean_score(x, ii), mean_score(x, indices[[a]]))
        }
      }
    }
    # A typed empty table supports metadata containing only single-item scales.
    z <- if (length(local)) do.call(rbind, local) else data.frame(
      group = integer(), scale = character(), hypothesis = character(),
      include_98 = logical(), type = character(), left = character(), right = character(),
      deciding = logical(), n = integer(), r = double(), lower = double(),
      upper = double(), evidence = character())
    correlations <- append_row(correlations, z)
    resolved <- character()
    for (s in scales) {
      zz <- z[z$scale == s, , drop = FALSE]
      primary <- decide(zz[!zz$include_98, , drop = FALSE])
      sensitivity <- decide(zz[zz$include_98, , drop = FALSE])
      if (kinds[s] == "negative") primary <- sensitivity <- "ANCHOR_DIRECTION_ASSUMED"
      if (kinds[s] == "exception") primary <- sensitivity <- "EXEMPT_FROM_PROBLEM_SCORING"
      final <- primary
      if (primary %in% hypotheses && primary != sensitivity)
        final <- "SENSITIVE_TO_NO_PROBLEM_CODES"
      classifications <- append_row(classifications, data.frame(group = g, scale = s,
        type = unname(kinds[s]), primary = primary, sensitivity = sensitivity,
        classification = final))
      if (final %in% hypotheses) resolved <- c(resolved, final)
      for (j in indices[[s]]) {
        action <- if (m$scoring_exception[j] || !m$positive[j]) "KEEP" else if (final == "RAW_AGREEMENT") "REVERSE" else
          if (final == "ALREADY_HIGHER_IS_WORSE") "KEEP" else "UNRESOLVED"
        chosen <- if (final %in% hypotheses) final else
          if (kinds[s] == "negative") "ALREADY_HIGHER_IS_WORSE" else NA_character_
        flag <- NA
        if (!is.na(chosen)) {
          bad <- zz[zz$hypothesis == chosen & zz$evidence == "negative" &
                      zz$type %in% c("item_pair", "item_rest"), , drop = FALSE]
          flag <- any(bad$left == m$item[j] | (bad$type == "item_pair" & bad$right == m$item[j]))
        }
        transformations <- append_row(transformations, data.frame(group = g,
          scale = s, item = m$item[j], column = m$column[j], positive = m$positive[j],
          present = m$present[j], action = action, item_conflict = flag))
      }
    }
    groups$possible_partial_reversal[g] <- length(unique(resolved)) > 1L
  }
  classification_table <- do.call(rbind, classifications)
  decisions <- classification_table[classification_table$classification %in% hypotheses, ]
  across <- nrow(groups) > 1L && any(vapply(split(decisions$classification, decisions$scale),
                                           function(x) length(unique(x)) > 1L, logical(1)))
  correlation_table <- do.call(rbind, correlations)
  count_table <- do.call(rbind, counts)
  item_table <- do.call(rbind, transformations)
  # Only ordinary responses can establish the exported response direction.
  analysis_counts <- count_table[count_table$scope == "analysis", ]
  observed <- analysis_counts$ordinary[match(
    paste(item_table$group, item_table$item),
    paste(analysis_counts$group, analysis_counts$item))] > 0L
  eligible <- kinds %in% "mixed" | names(kinds) %in% names(positive_scale_anchors)
  scale_key <- paste(classification_table$group, classification_table$scale)
  item_scale_key <- paste(item_table$group, item_table$scale)
  assessed <- scale_key %in% item_scale_key[
    item_table$positive & observed & eligible[match(item_table$scale, names(kinds))]]
  item_table$classification <- classification_table$classification[
    match(item_scale_key, scale_key)]
  item_table$status <- "not assessed"
  in_scope <- observed & item_scale_key %in% scale_key[assessed]
  item_table$status[observed & !item_table$positive] <- "assumed consistent"
  item_table$status[in_scope & item_table$positive] <- "unresolved"
  item_table$status[in_scope & item_table$positive &
                      item_table$classification == "RAW_AGREEMENT"] <- "not consistent"
  item_table$status[in_scope & item_table$positive &
                      item_table$classification == "ALREADY_HIGHER_IS_WORSE"] <- "consistent"
  item_table$status[in_scope & item_table$classification == "INCONSISTENT"] <- "mixed"
  item_table$status[observed & item_table$item_conflict %in% TRUE] <- "mixed"
  item_table$status[item_table$item %in% m$item[m$scoring_exception]] <- "not assessed"

  summarize_status <- function(decisions, conflict = FALSE) {
    resolved <- unique(decisions[decisions %in% hypotheses])
    if (conflict || "INCONSISTENT" %in% decisions || length(resolved) > 1L)
      return("mixed")
    if (!length(decisions) || any(!decisions %in% hypotheses)) return(NA_character_)
    if (resolved == "RAW_AGREEMENT") "not consistent" else "consistent"
  }
  groups$status <- vapply(groups$group, function(g) {
    summarize_status(classification_table$classification[
      classification_table$group == g & assessed],
      any(item_table$group == g & item_table$status == "mixed"))
  }, character(1))
  known <- unique(groups$status[!is.na(groups$status)])
  status <- if ("mixed" %in% known || length(known) > 1L) "mixed" else
    if (anyNA(groups$status) || !length(known)) NA_character_ else known

  issue_rows <- list()
  add_issue <- function(group, scale, classification, issue) {
    issue_rows[[length(issue_rows) + 1L]] <<- data.frame(
      group = group, scale = scale, classification = classification,
      issue = issue, stringsAsFactors = FALSE
    )
  }
  unresolved_groups <- groups$group[is.na(groups$status)]
  for (g in unresolved_groups) {
    in_group <- classification_table$group == g & assessed
    unresolved <- in_group &
      !classification_table$classification %in% hypotheses
    if (!any(in_group)) {
      add_issue(
        g, NA_character_, "NO_ASSESSABLE_SCALE",
        paste0(
          "No mixed-wording or configured positive scale had an observed ",
          "ordinary response on a positive item."
        )
      )
      next
    }
    for (row in which(unresolved)) {
      scale <- classification_table$scale[row]
      classification <- classification_table$classification[row]
      if (classification == "SENSITIVE_TO_NO_PROBLEM_CODES") {
        add_issue(
          g, scale, classification,
          "The conclusion changed when -98 responses were recoded as 1."
        )
        next
      }
      zz <- correlation_table[
        correlation_table$group == g & correlation_table$scale == scale &
          correlation_table$deciding & !correlation_table$include_98,
        , drop = FALSE
      ]
      if (!nrow(zz)) {
        reason <- "No deciding item-pair or scale-anchor comparison was available."
      } else if (all(zz$n < min_n)) {
        max_n <- if (length(zz$n)) max(zz$n, na.rm = TRUE) else 0L
        reason <- paste0(
          "Too few complete response pairs: maximum n = ", max_n,
          "; min_n = ", min_n, "."
        )
      } else {
        usable <- zz[zz$n >= min_n, , drop = FALSE]
        if (all(is.na(usable$r))) {
          reason <- "The compared responses had no variation, so correlations could not be calculated."
        } else if (all(is.na(usable$r) | abs(usable$r) < min_abs_r)) {
          max_r <- max(abs(usable$r), na.rm = TRUE)
          reason <- paste0(
            "Correlations were too weak: maximum |r| = ",
            format(round(max_r, 3), nsmall = 3),
            "; required |r| >= ", min_abs_r, "."
          )
        } else if (!any(usable$evidence %in% c("positive", "negative"))) {
          reason <- paste0(
            round(conf_level * 100),
            "% correlation intervals included zero."
          )
        } else {
          reason <- "The deciding correlations did not support one coding convention."
        }
      }
      add_issue(g, scale, classification, reason)
    }
  }
  issues <- if (length(issue_rows)) do.call(rbind, issue_rows) else data.frame(
    group = integer(), scale = character(), classification = character(),
    issue = character(), stringsAsFactors = FALSE
  )

  if (!verbose) {
    if (is.na(status)) {
      labels <- vapply(seq_len(nrow(issues)), function(i) {
        location <- paste0("group ", issues$group[i])
        if (!is.na(issues$scale[i]))
          location <- paste0(location, ", ", issues$scale[i])
        paste0("- ", location, ": ", issues$issue[i])
      }, character(1))
      shown <- head(labels, 6L)
      if (length(labels) > length(shown))
        shown <- c(shown, paste0("- ", length(labels) - length(shown),
                                 " additional issue(s)."))
      warning(paste(
        c("Insufficient or unresolved evidence for a dataset verdict.", shown,
          "Run audit_nf_reverse(..., verbose = TRUE)$issues for the full explanation."),
        collapse = "\n"
      ), call. = FALSE)
    }
    return(status)
  }
  list(status = status, issues = issues, items = item_table,
       classifications = classification_table,
       correlations = correlation_table, counts = count_table,
       transformations = do.call(rbind, transformations), groups = groups,
       mapping = m, unmapped_columns = setdiff(names(dat), c(m$column, controls)),
       settings = list(anchor_scales = anchor_scales,
         positive_scale_anchors = positive_scale_anchors, group_vars = group_vars,
         patient_id = patient_id, order_by = order_by, min_n = min_n,
         min_abs_r = min_abs_r, conf_level = conf_level,
         min_items = min_items, min_fraction = min_fraction,
         differing_conventions_across_groups = across))
}
