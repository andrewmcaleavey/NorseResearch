# utility functions, mostly developed for HV analysis 2025.

#' Collapse suffixed _A# variables and widen M-measure items (stable row order)
#'
#' This function:
#' 1) Collapses columns that match a suffix pattern (default: `"_A\\d+$"`) into their base
#'    names using ordered `coalesce` (higher digits preferred) with optional fallback to an
#'    existing base column.
#' 2) Auto-detects the measure code column: `"Kortkode"` or `"Short_code"`, copies it to a
#'    stable internal column `.code`, and uses that for filtering and reshaping.
#' 3) Treats columns matching `measure_prefix` (default: `"^M\\d+_"`) as measures; melts
#'    long, filters rows where `.code` equals the measure prefix (e.g., `"M1172"`), then casts
#'    wide so each item (substring after first underscore) becomes a column.
#' 4) **Critically:** enforces final row order so that within each `Respondent_ID`, rows are
#'    ordered by `Innsendt` (if present) or else `Submitted`. If neither time column exists
#'    or `Respondent_ID` is missing, a warning is issued and ordering is skipped.
#'
#' @param df A data.frame or tibble.
#' @param drop_suffix_pattern Regex of suffixed columns to collapse. Default `"_A\\d+$"`.
#' @param measure_prefix Regex of measure columns. Default `"^M\\d+_"`.
#' @param item_keep_pattern Optional regex to keep only certain item codes after melt
#'   (e.g., `"^Q\\d+[a-z]?$"`). If NULL, keep all.
#' @param fun.aggregate Optional function for \code{data.table::dcast} to resolve duplicates.
#' @param return_tibble Logical; return tibble if TRUE, else data.frame. Default TRUE.
#' @param coerce_wide_numeric Logical; try to coerce wide columns to numeric. Default FALSE.
#' @param respondent_col Name of the respondent identifier column. Default `"Respondent_ID"`.
#'
#' @return A tibble (default) or data.frame with suffixed columns collapsed, measures widened by item,
#'   and rows ordered within each Respondent_ID by `Innsendt`/`Submitted` when available.
#'
#' @importFrom dplyr mutate select coalesce
#' @importFrom rlang syms .data
#' @importFrom stringr str_remove str_extract
#' @importFrom tidyselect matches
#' @importFrom data.table as.data.table melt dcast rbindlist setnames set setorderv
#' @importFrom data.table fcoalesce
#' @importFrom tibble as_tibble
#' @importFrom stats as.formula
#' @export
collapse_measures_wide <- function(
    df,
    drop_suffix_pattern = "_A\\d+$",
    measure_prefix      = "^M\\d+_",
    item_keep_pattern   = NULL,
    fun.aggregate       = NULL,
    return_tibble       = TRUE,
    coerce_wide_numeric = FALSE,
    respondent_col      = "Respondent_ID"
) {
  stopifnot(is.data.frame(df))
  df <- df |> dplyr::select(where(function(x) !all(is.na(x))))

  # ---- Step 0: Detect code column and create stable `.code` ----
  has_kort  <- "Kortkode"    %in% names(df)
  has_short <- "Short_code"  %in% names(df)
  if (!has_kort && !has_short) {
    stop("Neither 'Kortkode' nor 'Short_code' found in dataset.")
  }
  code_src <- if (has_kort) "Kortkode" else "Short_code"

  # ---- Step 1: Collapse *_A# columns into base names ----
  data <- df
  a_vars <- grep(drop_suffix_pattern, names(data), value = TRUE)
  if (length(a_vars) > 0) {
    bases <- unique(stringr::str_remove(a_vars, drop_suffix_pattern))
    for (b in bases) {
      b_cols <- grep(paste0("^", b, drop_suffix_pattern), names(data), value = TRUE)
      ord <- order(as.numeric(stringr::str_extract(b_cols, "(?<=_A)\\d+$")), decreasing = TRUE)
      b_cols <- b_cols[ord]

      if (b %in% names(data)) {
        data <- dplyr::mutate(data, !!b := dplyr::coalesce(!!!rlang::syms(b_cols), .data[[b]]))
      } else {
        data <- dplyr::mutate(data, !!b := dplyr::coalesce(!!!rlang::syms(b_cols)))
      }
    }
    data <- dplyr::select(data, -tidyselect::matches(drop_suffix_pattern))
  }

  # ---- Step 2: Melt/filter/cast per measure using `.code` ----
  DT <- data.table::as.data.table(data)
  DT[, .code := as.character(get(code_src))]
  # ---- Step 1.5: Auto-coerce Q* items to numeric (to avoid melt() → character) ----
  auto_coerce_q_numeric <- TRUE              # expose as a function argument if you like
  q_numeric_threshold   <- 0.90              # expose as argument
  q_item_pattern        <- "^Q"              # expose as argument
  q_na_ok_chars         <- c("", "NA", "N/A", ".", " ", "NULL")  # expose as argument
  q_allow_decimal_comma <- TRUE              # expose as argument
  coercion_log <- list(coerced = character(0), skipped = character(0))

  q_cols <- grep(paste0("^(", sub("_$", "", measure_prefix), ")_", q_item_pattern), names(DT), value = TRUE)

  if (isTRUE(auto_coerce_q_numeric) && length(q_cols)) {
    for (nm in q_cols) {
      x <- DT[[nm]]
      if (is.numeric(x)) next

      xx <- as.character(x)
      xx[xx %in% q_na_ok_chars] <- NA_character_
      xx <- trimws(xx)
      if (isTRUE(q_allow_decimal_comma)) xx <- gsub(",", ".", xx, fixed = TRUE)

      suppressWarnings(num <- as.numeric(xx))
      denom <- sum(!is.na(xx))
      frac  <- if (denom == 0) 1 else mean(!is.na(num) & !is.na(xx))

      if (is.na(frac)) frac <- 0
      if (frac >= q_numeric_threshold) {
        data.table::set(DT, j = nm, value = num)
        coercion_log$coerced <- c(coercion_log$coerced, nm)
      } else {
        coercion_log$skipped <- c(coercion_log$skipped, sprintf("%s (%.2f < %.2f)", nm, frac, q_numeric_threshold))
      }
    }
  }
  # Only treat names like "M####_..." as measure cols (must have something after underscore)
  measure_cols <- grep(paste0("^(", sub("_$", "", measure_prefix), ")_.+"), names(DT), value = TRUE)
  if (length(measure_cols) == 0L) {
    DT[, .code := NULL]
    # Final ordering (no cast) if possible
    DT <- enforce_within_id_order(DT, respondent_col)
    return(if (return_tibble) tibble::as_tibble(DT) else as.data.frame(DT))
  }

  # id columns = everything not a measure column and not `.code`
  id_cols <- setdiff(names(DT), c(measure_cols, ".code"))

  # Compute measure codes like "M1172"
  measures <- unique(sub("^(M\\d+)_.*", "\\1", measure_cols))

  result_list <- vector("list", length(measures))
  names(result_list) <- measures

  for (m in measures) {
    m_cols <- grep(paste0("^", m, "_"), names(DT), value = TRUE)
    if (length(m_cols) == 0L) next

    keep_cols <- unique(c(id_cols, ".code", m_cols))
    DT_sub <- DT[, ..keep_cols]

    # De-dup any accidental name collisions before melt
    if (any(duplicated(names(DT_sub)))) {
      data.table::setnames(DT_sub, make.unique(names(DT_sub), sep = "."))
      # Ensure .code still present under that exact name
      if (!".code" %in% names(DT_sub)) {
        alt <- names(DT_sub)[startsWith(names(DT_sub), ".code.")]
        if (length(alt) >= 1L) data.table::setnames(DT_sub, alt[1L], ".code")
      }
    }

    m_long <- data.table::melt(
      DT_sub,
      id.vars       = c(id_cols, ".code"),
      measure.vars  = m_cols,
      variable.name = "Measure_Item",
      value.name    = "Value"
    )
    # Extract item as the substring after the first underscore
    m_long[, Item := sub("^[^_]+_", "", Measure_Item)]

    # Optional: keep only items that match a pattern (e.g., Q-codes)
    if (!is.null(item_keep_pattern)) {
      m_long <- m_long[grepl(item_keep_pattern, Item)]
    }

    # Keep rows where .code equals the current measure (e.g., "M1172")
    m_long <- m_long[.code == m]

    # Clean up helpers
    m_long[, c("Measure_Item", ".code") := NULL]

    result_list[[m]] <- m_long
  }

  df_long <- data.table::rbindlist(result_list, use.names = TRUE, fill = TRUE)

  # If nothing left (e.g., no items matched), return original (minus helper)
  DT[, .code := NULL]
  if (nrow(df_long) == 0L) {
    DT <- enforce_within_id_order(DT, respondent_col)
    return(if (return_tibble) tibble::as_tibble(DT) else as.data.frame(DT))
  }

  # Rebuild ID set from post-collapse data (exclude measure cols, which are M####_* names)
  id_cols <- setdiff(names(data), grep(paste0("^(", sub("_$", "", measure_prefix), ")_.+"), names(data), value = TRUE))
  if (length(id_cols) == 0L) {
    df_long[, .row_id__ := seq_len(.N)]
    id_cols <- ".row_id__"
    synthetic_id <- TRUE
  } else synthetic_id <- FALSE

  # Cast: id1 + id2 + ... ~ Item
  id_cols_quoted <- paste0("`", id_cols, "`")
  cast_formula <- stats::as.formula(paste(paste(id_cols_quoted, collapse = " + "), "~ Item"))

  if (is.null(fun.aggregate)) {
    df_wide <- data.table::dcast(df_long, formula = cast_formula, value.var = "Value")
  } else {
    df_wide <- data.table::dcast(df_long, formula = cast_formula, value.var = "Value", fun.aggregate = fun.aggregate)
  }

  if (isTRUE(synthetic_id) && ".row_id__" %in% names(df_wide)) df_wide[, .row_id__ := NULL]

  # Optional: try to coerce wide columns to numeric when safe
  if (isTRUE(coerce_wide_numeric)) {
    non_id <- setdiff(names(df_wide), id_cols)
    for (nm in non_id) {
      suppressWarnings({ as_num <- as.numeric(df_wide[[nm]]) })
      if (!all(is.na(as_num)) || all(is.na(df_wide[[nm]]))) data.table::set(df_wide, j = nm, value = as_num)
    }
  }

  # ---- Step: Merge Qxxx_1 into Qxxx, then drop Qxxx_1 ----
  q1_cols <- grep("^Q\\d+_1$", names(df_wide), value = TRUE)
  if (length(q1_cols)) {
    for (q1 in q1_cols) {
      base <- sub("_1$", "", q1)
      if (base %in% names(df_wide)) {
        # Prefer existing base; fill its gaps from *_1, then drop *_1
        data.table::set(df_wide, j = base,
                        value = data.table::fcoalesce(df_wide[[base]], df_wide[[q1]]))
        data.table::set(df_wide, j = q1, value = NULL)
      } else {
        # No base yet: simple rename *_1 -> base
        data.table::setnames(df_wide, q1, base)
      }
    }
  }


  # ---- Step 3: ENFORCE FINAL ORDER within each Respondent_ID by Innsendt/Submitted ----
  df_wide <- enforce_within_id_order(df_wide, respondent_col)

  if (return_tibble) tibble::as_tibble(df_wide) else as.data.frame(df_wide)
  attr_obj <- list(
    q_coercion = coercion_log
  )
  # if you're returning a tibble:
  out <- if (return_tibble) tibble::as_tibble(df_wide) else as.data.frame(df_wide)
  attr(out, "q_numeric_coercion") <- attr_obj
  return(out)
}

# ---- Helper: enforce order within Respondent_ID by Innsendt/Submitted
# internal helper, not exported
enforce_within_id_order <- function(DT, respondent_col = "Respondent_ID", time_order = c("asc","desc")) {
  time_order <- match.arg(time_order)
  DT <- data.table::as.data.table(DT)

  # choose time column: Innsendt preferred, else Submitted
  time_col <- if ("Innsendt" %in% names(DT)) {
    "Innsendt"
  } else if ("Submitted" %in% names(DT)) {
    "Submitted"
  } else {
    NULL
  }

  if (is.null(time_col) || !(respondent_col %in% names(DT))) {
    warning("Ordering skipped: missing '", respondent_col, "' and/or 'Innsendt'/'Submitted'.")
    return(DT[])
  }

  x <- DT[[time_col]]

  # if already Date/POSIX, use directly
  if (inherits(x, "POSIXt")) {
    t_key <- x
  } else if (inherits(x, "Date")) {
    t_key <- as.POSIXct(x)
  } else {
    # robust parse for common Norwegian/ISO formats
    xx <- as.character(x)

    # try a set of formats; fill in whatever parses at each step
    fmts <- c(
      "%Y-%m-%d %H:%M:%OS", "%Y-%m-%d %H:%M", "%Y-%m-%d",
      "%d.%m.%Y %H:%M:%OS", "%d.%m.%Y %H:%M", "%d.%m.%Y",
      "%Y/%m/%d %H:%M:%OS", "%Y/%m/%d %H:%M", "%Y/%m/%d",
      "%d.%m.%y %H:%M:%OS", "%d.%m.%y %H:%M", "%d.%m.%y"
    )

    t_key <- as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC")
    t_key <- rep(t_key, length(xx))

    for (fmt in fmts) {
      suppressWarnings({
        p <- as.POSIXct(xx, format = fmt, tz = "UTC")
      })
      # fill only where still NA and parsing succeeded
      fill <- is.na(t_key) & !is.na(p)
      if (any(fill)) t_key[fill] <- p[fill]
      if (all(!is.na(t_key))) break
    }

    # epoch seconds as last resort
    if (all(is.na(t_key))) {
      suppressWarnings({
        num <- as.numeric(xx)
      })
      if (any(!is.na(num))) {
        base <- as.POSIXct(0, origin = "1970-01-01", tz = "UTC")
        t_key <- base + num
      }
    }

    # absolute last fallback: lexicographic (stable but not chronological)
    if (all(is.na(t_key))) t_key <- xx
  }

  # build sort spec
  ord_cols <- c(respondent_col, ".__ord_time__")
  ord_dirs <- c(+1L, if (identical(time_order, "asc")) +1L else -1L)

  DT[, .__ord_time__ := t_key]
  data.table::setorderv(DT, cols = ord_cols, order = ord_dirs, na.last = TRUE)
  DT[, .__ord_time__ := NULL]
  DT[]
}


#' Rename variables to English equivalents for export
#'
#' This function renames specific variables in a dataset from Norwegian to
#' English for consistency in exported files. If a variable with the target
#' English name already exists in the dataset, the renaming for that variable
#' is skipped to avoid overwriting.
#'
#' @param dat A data frame containing variables with Norwegian names.
#'
#' @return A data frame with selected variables renamed to English,
#'   where possible. Variables that would overwrite existing names are left
#'   unchanged.
#'
#' @details
#' The following renamings are attempted:
#' \itemize{
#'   \item \code{Pasientid} → \code{Respondent_ID}
#'   \item \code{Skjemapakke} → \code{Measurepackage}
#'   \item \code{Skjema} → \code{Measure_name}
#'   \item \code{Kortkode} → \code{Short_code}
#'   \item \code{Innsendt} → \code{Submitted}
#'   \item \code{Varighet} → \code{Duration}
#' }
#'
#' If any of the target English names already exist in \code{dat}, those specific
#' renamings are skipped silently.
#'
#' @examples
#' dat <- data.frame(
#'   Pasientid = 1:3,
#'   Skjema = c("A", "B", "C"),
#'   Varighet = c(10, 20, 30),
#'   Respondent_ID = c("X", "Y", "Z") # already exists
#' )
#' make_english_export(dat)
#'
#' @export
make_english_export <- function(dat) {
  lookup <- c(
    Respondent_ID = "Pasientid",
    Measurepackage  = "Skjemapakke",
    Measure_name = "Skjema",
    Measure_name = "Skjemanavn",
    Short_code = "Kortkode",
    Submitted = "Innsendt",
    Duration = "Varighet",
    Duration = "Varigheit"
  )

  # Drop any mappings where the target name already exists in dat
  lookup <- lookup[!names(lookup) %in% names(dat)]

  dplyr::rename(dat, dplyr::any_of(lookup))
}

#' Create formatted GT tables for factor analysis results
#'
#' @description
#' Given a fitted factor model from **psych** (`psych::fa()`), this function
#' builds two nicely formatted [`gt`](https://gt.rstudio.com/) tables:
#'
#' 1) **Indicator table** (`ind_table`): item (indicator) loadings per factor,
#'    with communalities, uniquenesses, and complexity. Small loadings are
#'    grey/italic; cross-loadings are italic; non-assignable indicators (unclear
#'    loadings) are highlighted.
#'
#' 2) **Factor summary table** (`f_table`): eigenvalues and variance explained
#'    (from `x[["Vaccounted"]]`), and—when there are multiple factors—the factor
#'    correlation matrix `Phi`.
#'
#' @param x An object returned by `psych::fa()` (i.e., a factor analysis fit
#'   from the **psych** package). Must contain `loadings`, `communality`,
#'   `uniquenesses`, `complexity`, `Vaccounted`, and (for \(>1\) factors) `Phi`.
#' @param varlabels Optional character vector of length equal to the number of
#'   rows in `x$loadings`. Used as human-readable indicator names (row labels).
#'   If `sort = TRUE`, labels are re-ordered to match the sorted loadings.
#' @param title Character string used as the title of the indicator table.
#'   Default is `"Factor analysis results"`.
#' @param diffuse Numeric tolerance in \[0, 1\] for determining *non-assignable*
#'   indicators (items with unclear loadings). If the absolute difference
#'   between the two highest absolute loadings on an item is *less than*
#'   `diffuse`, the item is marked as non-assignable (row highlighted). Default
#'   is `0.10`.
#' @param small Numeric threshold in \[0, 1\] below which absolute factor
#'   loadings are styled grey and italic to indicate small magnitude. Default
#'   is `0.30`.
#' @param cross Numeric threshold in \[0, 1\] above which *secondary* (off-factor)
#'   loadings are styled italic to indicate cross-loadings. Default is `0.20`.
#' @param sort Logical; if `TRUE` (default), the loadings are sorted using
#'   `psych::fa.sort()` and `varlabels` are re-ordered accordingly.
#'
#' @details
#' Factor columns are auto-named as `"Factor_1"`, `"Factor_2"`, … in the
#' returned tables. Styling rules applied to the indicator table:
#'
#' * **Small loadings**: `abs(loading) < small` → grey + italic text.
#' * **Cross-loadings** (when `n_factors > 1`): `abs(loading) > cross` on any
#'   non-primary factor for that item → italic text.
#' * **Non-assignable items**: if the gap between the two largest absolute
#'   loadings is `< diffuse` → row filled red (indicates unclear factor
#'   assignment).
#'
#' The factor summary table stacks `Vaccounted` (eigenvalues and variance
#' explained) and, if available, `Phi` (factor correlations), with values
#' rounded for readability.
#'
#' @return
#' A named list with two `gt_tbl` objects:
#' * `ind_table`: GT table of indicators (rows) by factors (columns), plus
#'   `Communality`, `Uniqueness`, and `Complexity`.
#' * `f_table`: GT table of eigenvalues/variance explained and, if applicable,
#'   factor correlations.
#'
#' @section Warnings:
#' If `varlabels` is supplied but its length differs from `nrow(x$loadings)`,
#' a warning is issued and labels are not applied.
#'
#' @examples
#' \dontrun{
#'   library(psych)
#'   set.seed(1)
#'   # Toy data with 3 factors
#'   X <- simulatePolychoric(300, 3, 5)$data
#'   fa_fit <- fa(X, nfactors = 3, rotate = "oblimin")
#'
#'   # Optional labels
#'   labs <- paste("Item", seq_len(nrow(fa_fit$loadings)))
#'
#'   out <- fa_table(
#'     x = fa_fit,
#'     varlabels = labs,
#'     title = "EFA results (oblimin)",
#'     diffuse = 0.10,
#'     small   = 0.30,
#'     cross   = 0.20,
#'     sort    = TRUE
#'   )
#'
#'   # Print the GT tables
#'   out$ind_table
#'   out$f_table
#' }
#'
#' @seealso [psych::fa()], [psych::fa.sort()], [gt::gt()]
#'
#' @importFrom dplyr tibble rename mutate across starts_with
#' @importFrom purrr map map2
#' @importFrom tibble rownames_to_column
#' @importFrom gt gt tab_header tab_style cells_body cell_text cell_fill
#' @export
fa_table <- function(x, varlabels = NULL, title = "Factor analysis results", diffuse = .10, small = .30, cross = .20, sort = TRUE) {
  #get sorted loadings
  require(dplyr)
  require(purrr)
  require(tibble)
  require(gt)
  if(sort == TRUE) {
    x <- psych::fa.sort(x)
  }
  if(!is.null(varlabels)) {
    if(length(varlabels) != nrow(x$loadings)) { warning("Number of variable labels and number of variables are unequal. Check your input!",
                                                        call. = FALSE) }
    if(sort == TRUE) {
      varlabels <- varlabels[x$order]
    }
  }
  if(is.null(varlabels)) {varlabels <- rownames(x$loadings)}

  loadings <- data.frame(unclass(x$loadings))

  #make nice names
  factornamer <- function(nfactors) {
    paste0("Factor_", 1:nfactors)}

  nfactors <- ncol(loadings)
  fnames <- factornamer(nfactors)
  names(loadings) <- fnames

  # prepare locations
  factorindex <- apply(loadings, 1, function(x) which.max(abs(x)))

  # adapted from from sjplot: getremovableitems
  getRemovableItems <- function(dataframe, fctr.load.tlrn = diffuse) {
    # clear vector
    removers <- vector(length = nrow(dataframe))
    # iterate each row of the data frame. each row represents
    # one item with its factor loadings
    for (i in seq_along(removers)) {
      # get factor loadings for each item
      rowval <- as.numeric(abs(dataframe[i, ]))
      # retrieve highest loading
      maxload <- max(rowval)
      # retrieve 2. highest loading
      max2load <- sort(rowval, TRUE)[2]
      # check difference between both
      if (abs(maxload - max2load) < fctr.load.tlrn) {
        # if difference is below the tolerance,
        # remeber row-ID so we can remove that items
        # for further PCA with updated data frame
        removers[i] <- TRUE
      }
    }
    # return a vector with index numbers indicating which items
    # have unclear loadings
    return(removers)
  }
  if(nfactors > 1) {
    removable <- getRemovableItems(loadings)
    cross_loadings <- purrr::map2(fnames, seq_along(fnames), function(f, i) {
      (abs(loadings[,f] > cross)) & (factorindex != i)
    })
  }

  small_loadings <- purrr::map(fnames, function(f) {
    abs(loadings[,f]) < small
  })

  ind_table <- dplyr::tibble(varlabels, loadings) %>%
    dplyr::rename(Indicator = varlabels) %>%
    dplyr::mutate(Communality = x$communality, Uniqueness = x$uniquenesses, Complexity = x$complexity) %>%
    dplyr::mutate(across(starts_with("Factor"), round, 3))  %>%
    dplyr::mutate(across(c(Communality, Uniqueness, Complexity), round, 2))


  ind_table <- ind_table %>% gt(rowname_col = "Indicator") %>% tab_header(title = title)
  # mark small loadiongs
  for(f in seq_along(fnames)) {
    ind_table <- ind_table %>%  tab_style(style = cell_text(color = "#D3D3D3", style = "italic"),
                                          locations = cells_body(columns = fnames[f], rows = small_loadings[[f]]))
  }
  # mark cross loadings

  if (nfactors > 1) {
    for (f in seq_along(fnames)) {
      ind_table <-
        ind_table %>%  tab_style(
          style = cell_text(style = "italic"),
          locations = cells_body(columns = fnames[f], rows = cross_loadings[[f]])
        )
    }
    # mark non-assignable indicators
    ind_table <-
      ind_table %>%  tab_style(style = cell_fill(color = "#D93B3B"),
                               locations = cells_body(rows = removable))
  }

  # adapted from https://www.anthonyschmidt.co/post/2020-09-27-efa-tables-in-r/
  Vaccounted <- x[["Vaccounted"]]
  colnames(Vaccounted) <- fnames
  if (nfactors > 1) {
    Phi <- x[["Phi"]]
    rownames(Phi) <- fnames
    colnames(Phi) <- fnames
    f_table <- rbind(Vaccounted, Phi) %>%
      as.data.frame() %>%
      rownames_to_column("Property") %>%
      mutate(across(where(is.numeric), round, 3)) %>%
      gt() %>% tab_header(title = "Eigenvalues, Variance Explained, and Factor Correlations for Rotated Factor Solution")
  }
  else if(nfactors == 1) {
    f_table <- rbind(Vaccounted) %>%
      as.data.frame() %>%
      rownames_to_column("Property") %>%
      mutate(across(where(is.numeric), round, 3)) %>%
      gt() %>% tab_header(title = "Eigenvalues, Variance Explained, and Factor Correlations for Rotated Factor Solution")
  }

  return(list("ind_table" = ind_table, "f_table" = f_table))

}

#' Repair likely mojibake in character columns (UTF-8 mis-decoded as Latin-1/CP1252)
#'
#' @description
#' Fixes common mojibake such as `"Helse FÃ¸rde"` (should be `"Helse Førde"`)
#' caused by UTF-8 bytes being interpreted as Latin-1 / Windows-1252 at import.
#'
#' Robust to real-world messy data: handles strings that are not valid UTF-8 by
#' performing detection in byte mode and using byte-safe summarization of changes.
#'
#' @param data A data.frame (or tibble).
#' @param cols Optional tidyselect-style specification of columns to check. If
#'   \code{NULL} (default), all character columns are checked.
#' @param from Candidate single-byte encodings to try for the byte round-trip.
#'   Defaults to \code{c("latin1", "Windows-1252")}.
#' @param patterns Regex patterns treated as signals of mojibake.
#' @param quiet If \code{TRUE}, suppress console reporting.
#' @param report Console output mode: \code{"unique"} (default), \code{"columns"},
#'   \code{"both"}, or \code{"none"}.
#' @param max_unique Maximum number of unique change rows to print (most frequent first).
#' @param sanitize_invalid If \code{TRUE} (default), attempts to sanitize invalid
#'   UTF-8 strings for safer processing using \code{iconv(sub="byte")}.
#'   This is conservative: it preserves problematic bytes as <xx> escapes.
#'
#' @return A data.frame of the same class as \code{data} with repaired character columns.
#'
#' @examples
#' df <- data.frame(
#'   org  = c("Helse FÃ¸rde", "Helse Vest", NA),
#'   note = c("smÃ¥", "OK", "Ã…lesund"),
#'   stringsAsFactors = FALSE
#' )
#' fix_failed_encoding(df)
#'
#' @export
fix_failed_encoding <- function(data,
                                cols = NULL,
                                from = c("latin1", "Windows-1252"),
                                patterns = c(
                                  "Ã.", "Â.", "â€™", "â€œ", "â€\u009D", "â€“", "â€”", "â€¦",
                                  "Ã¥", "Ã¸", "Ã¦", "Ã…", "Ã˜", "Ã†"
                                ),
                                quiet = FALSE,
                                report = c("unique", "columns", "both", "none"),
                                max_unique = 50,
                                sanitize_invalid = TRUE) {

  report <- match.arg(report)
  if (report == "none") quiet <- TRUE

  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame (or tibble).", call. = FALSE)
  }

  # Select columns: tidyselect if available; else accept character vector; else default.
  cn <- names(data)
  if (is.null(cols)) {
    target_cols <- cn[vapply(data, is.character, logical(1))]
  } else {
    if (requireNamespace("tidyselect", quietly = TRUE) &&
        requireNamespace("rlang", quietly = TRUE)) {
      sel <- tidyselect::eval_select(rlang::enquo(cols), data)
      target_cols <- names(sel)
    } else {
      if (!is.character(cols)) {
        stop("`cols` requires tidyselect+rlang or a character vector of column names.", call. = FALSE)
      }
      target_cols <- cols
    }
  }

  if (length(target_cols) == 0L) {
    if (!quiet) message("fix_failed_encoding(): no character columns selected; returning input unchanged.")
    return(data)
  }

  # Sanitize invalid UTF-8 (optional): convert to UTF-8 and preserve invalid bytes as <xx>
  sanitize_vec <- function(x) {
    if (!sanitize_invalid) return(x)
    # iconv(from="") means "assume current encoding"; sub="byte" preserves bad bytes.
    y <- iconv(x, from = "", to = "UTF-8", sub = "byte")
    # iconv can return NA for some inputs; keep original in that case
    y[is.na(y) & !is.na(x)] <- x[is.na(y) & !is.na(x)]
    y
  }

  # Count mojibake signals in byte mode to avoid UTF-8 validation errors
  count_signals <- function(x) {
    x <- x[!is.na(x)]
    if (!length(x)) return(0L)
    x2 <- sanitize_vec(x)
    sum(vapply(
      patterns,
      function(p) sum(grepl(p, x2, perl = TRUE, useBytes = TRUE)),
      integer(1)
    ))
  }

  byte_roundtrip <- function(x, enc) {
    if (all(is.na(x))) return(x)

    `%||%` <- function(a, b) if (is.null(a)) b else a


    # CP1252 special mapping (Unicode code point -> byte), incl. U+02DC -> 0x98
    cp1252_map <- c(
      "8364" = 0x80, "8218" = 0x82, "402"  = 0x83, "8222" = 0x84,
      "8230" = 0x85, "8224" = 0x86, "8225" = 0x87, "710"  = 0x88,
      "8240" = 0x89, "352"  = 0x8A, "8249" = 0x8B, "338"  = 0x8C,
      "381"  = 0x8E, "8216" = 0x91, "8217" = 0x92, "8220" = 0x93,
      "8221" = 0x94, "8226" = 0x95, "8211" = 0x96, "8212" = 0x97,
      "732"  = 0x98, "8482" = 0x99, "353"  = 0x9A, "8250" = 0x9B,
      "339"  = 0x9C, "382"  = 0x9E, "376"  = 0x9F
    )

    cp1252_bytes_to_utf8 <- function(s) {
      ints <- utf8ToInt(s)
      rawv <- raw(0)

      for (u in ints) {
        if (u <= 0xFF) {
          rawv <- c(rawv, as.raw(u))
        } else {
          key <- as.character(u)
          b <- cp1252_map[key]
          if (is.na(b)) return(s) # can't represent -> bail out
          rawv <- c(rawv, as.raw(b))
        }
      }

      out <- rawToChar(rawv)
      Encoding(out) <- "UTF-8"
      out
    }

    out <- x
    idx <- which(!is.na(x))
    if (!length(idx)) return(out)

    if (identical(enc, "Windows-1252")) {
      out[idx] <- vapply(x[idx], cp1252_bytes_to_utf8, character(1), USE.NAMES = FALSE)
      return(out)
    }

    # latin1 path
    out[idx] <- vapply(
      x[idx],
      FUN = function(s) {
        s_sb <- iconv(s, from = "", to = enc, sub = NA)
        if (is.na(s_sb)) return(s)
        out2 <- rawToChar(charToRaw(s_sb))
        Encoding(out2) <- "UTF-8"
        out2
      },
      FUN.VALUE = character(1),
      USE.NAMES = FALSE
    )

    out
  }



  out <- data

  # Change log for unique-summary reporting
  change_col <- character(0)
  change_before <- character(0)
  change_after <- character(0)

  col_report <- list()

  for (col in target_cols) {
    x <- out[[col]]
    if (!is.character(x)) next

    before_signals <- count_signals(x)
    if (before_signals == 0L) next

    best <- list(enc = NA_character_, x = x, signals = before_signals, changed = 0L)

    for (enc in from) {
      x2 <- byte_roundtrip(x, enc)
      # Require valid UTF-8 after conversion
      valid_prop <- if (length(x2)) mean(utf8::utf8_valid(x2[!is.na(x2)])) else 1

      after_signals <- count_signals(x2)
      changed_n <- sum(!is.na(x) & !is.na(x2) & x != x2)

      # Prefer: (1) more valid UTF-8, then (2) fewer signals, then (3) more changes
      best_score <- c(best$valid_prop %||% -1, -best$signals, best$changed)
      cand_score <- c(valid_prop, -after_signals, changed_n)

      if (changed_n > 0L && (is.null(best$valid_prop) || any(cand_score > best_score))) {
        best <- list(enc = enc, x = x2, signals = after_signals, changed = changed_n, valid_prop = valid_prop)
      }

    }

    if (!is.na(best$enc)) {
      idx <- which(!is.na(x) & !is.na(best$x) & x != best$x)
      if (length(idx)) {
        change_col <- c(change_col, rep(col, length(idx)))
        change_before <- c(change_before, x[idx])
        change_after <- c(change_after, best$x[idx])
      }

      out[[col]] <- best$x

      # Row-level fallback: if any values are still invalid UTF-8, try the other encoding just for those
      bad <- !is.na(out[[col]]) & !utf8::utf8_valid(out[[col]])
      if (any(bad)) {
        other <- setdiff(from, best$enc)
        if (length(other)) {
          x_bad_fixed <- byte_roundtrip(out[[col]][bad], other[1])
          # only accept fallback where it becomes valid UTF-8
          ok <- !is.na(x_bad_fixed) & utf8::utf8_valid(x_bad_fixed)
          out[[col]][bad][ok] <- x_bad_fixed[ok]
        }
      }

      col_report[[col]] <- list(
        enc = best$enc,
        changed = best$changed,
        before_signals = before_signals,
        after_signals = best$signals
      )
    }
  }

  if (!quiet) {
    if (length(col_report) == 0L) {
      message("fix_failed_encoding(): no likely mojibake detected; no changes made.")
      return(out)
    }

    if (report %in% c("columns", "both")) {
      message("fix_failed_encoding(): repaired encoding in ", length(col_report), " column(s):")
      for (col in names(col_report)) {
        r <- col_report[[col]]
        message(
          "- ", col, ": ",
          r$changed, " value(s) changed; signals ", r$before_signals, " -> ", r$after_signals,
          "; byte round-trip via ", r$enc, " -> UTF-8."
        )
      }
    }

    if (report %in% c("unique", "both")) {
      # Aggregate unique (before, after) pairs safely without separator packing.
      if (length(change_before) == 0L) {
        message("fix_failed_encoding(): changes made, but no per-value differences were logged.")
        return(out)
      }

      # Sanitize for printing only (do not alter out)
      b_print <- sanitize_vec(change_before)
      a_print <- sanitize_vec(change_after)

      pairs <- data.frame(
        before = b_print,
        after  = a_print,
        stringsAsFactors = FALSE
      )

      # Use base aggregation (no dplyr dependency)
      counts <- aggregate(rep(1L, nrow(pairs)), by = pairs, FUN = sum)
      names(counts)[names(counts) == "x"] <- "n"
      counts <- counts[order(-counts$n), , drop = FALSE]

      message(
        "fix_failed_encoding(): unique changes (before -> after) across all checked columns: ",
        nrow(counts), " unique pair(s)."
      )

      n_show <- min(nrow(counts), max_unique)
      for (i in seq_len(n_show)) {
        message(sprintf('- %d× "%s"  ->  "%s"', counts$n[i], counts$before[i], counts$after[i]))
      }
      if (nrow(counts) > n_show) {
        message(sprintf("... (%d more unique change(s) not shown; increase `max_unique` to print more)",
                        nrow(counts) - n_show))
      }
    }
  }

  out
}
