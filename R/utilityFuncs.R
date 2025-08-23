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

# ---- Helper: enforce order within Respondent_ID by Innsendt/Submitted ----
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
    Short_code = "Kortkode",
    Submitted = "Innsendt",
    Duration = "Varighet"
  )

  # Drop any mappings where the target name already exists in dat
  lookup <- lookup[!names(lookup) %in% names(dat)]

  dplyr::rename(dat, dplyr::any_of(lookup))
}
