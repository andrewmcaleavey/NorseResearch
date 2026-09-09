## code to further anonymize `HF_research_data_2021_fscores`
##
## The base data (`data-raw/HF_research_data_2021.R`) already pseudonymizes the
## respondent / treatment / assessment identifiers, but the *timestamps* still
## leak real-world information: the exact day and time each assessment was
## completed can, combined with treatment setting, narrow down who a record
## belongs to. This script adds that extra layer of protection by perturbing
## every datetime variable.
##
## Strategy
##
## Each datetime variable is shifted by a **fixed, randomly-drawn offset that is
## constant within each respondent and within each treatment**:
##
##      - respondent shift: each anon_id gets one random shift (seconds), applied
##      to *all* of that respondent's timestamps.
##      - treatment shift: each anon_tx_id gets one additional random shift
##      (seconds), so the two treatments of the same person are not aligned.
##
## Because the offset is constant within a respondent/treatment, *internal*
## relationships that the rest of the analysis relies on are preserved:
##      - the ordering of a person's assessments,
##      - pt_wks_since_first / tx_wks_since_first (differences of dates, which are
##      invariant to a constant shift),
##      - the *_first_pt / first-value columns (order_by = date).
## pt_first_date / tx_first_date are recomputed below from the shifted `date`
## rather than shifted directly, so they stay consistent with `date`.
##
## What it destroys: the real calendar date and time of any assessment. With a
## large shift relative to the span of the data, no external reader can map a
## record back to a real clock time.
##
## Determinism: the whole thing is reproducible from a fixed seed, so the same
## offset is applied to every datetime column. Run this script *after* the
## `HF_research_data_2021_fscores` object has been built.

# require(lubridate)

## --- tunables: adjust to make the data unrecognizable while staying valid -----

# Seed so the shift is reproducible (regenerating the dataset gives the same result).
anonymize.seed <- 5237

# Per-respondent shift, sampled uniformly in +/- this many days.
respondent_shift_days <- 3000

# Per-treatment shift, sampled uniformly in +/- this many days.
treatment_shift_days <- 90

# How much of the original time-of-day to keep. 0 => strip it and replace it with
# a uniform-random time so the real submission time is not recoverable; 1 => keep
# the original time-of-day exactly; 0.5 => a 50/50 blend. Default 0 destroys it.
keep_time_of_day <- 0

## -----------------------------------------------------------------------------

set.seed(anonymize.seed)

## datetime variables to anonymize --------------------------------------------

# Derived / convenience timestamps. Do NOT shift these directly: they are
# recomputed from the shifted `date` below so they stay internally consistent.
derived_dt <- c("pt_first_date", "tx_first_date")

# All datetime variables are POSIXct/POSIXt.
all_dt <- names(HF_research_data_2021_fscores)[
  vapply(HF_research_data_2021_fscores,
         function(x) inherits(x, c("POSIXct", "POSIXt")),
         logical(1))
]

expected_dt <- c("date", "pt_first_date", "tx_first_date",
                 "assessment_instance_start_date",
                 "assessment_instance_end_date",
                 "assessment_instance_created_date",
                 "assessment_instance_last_modified_submitted",
                 "assessment_instance_first_time_started_date",
                 "assessment_instance_first_time_submitted_date",
                 "respondent_last_login")

if (!identical(all_dt, expected_dt)) {
  stop("datetime columns changed; update expected_dt in this script.
        found: ", paste(all_dt, collapse = ", "))
}

# Timestamps we shift directly.
source_dt <- setdiff(all_dt, derived_dt)

cat("Anonymizing ", length(source_dt), " datetime columns:\n",
    paste(source_dt, collapse = ", "), "\n")

## per-respondent / per-treatment offsets --------------------------------------

# One random shift (in seconds) per unique respondent / treatment, keyed on the
# already-pseudonymized IDs. This is what makes the shift *consistent* across a
# respondent's rows while still being effectively random from an outside view.
respondent_shift <- tibble(tibble(HF_research_data_2021_fscores) |>
                             dplyr::select(anon_id) |>
                             distinct()) |>
  mutate(resp_shift = runif(n(),
                            min = -respondent_shift_days * 86400,
                            max =  respondent_shift_days * 86400))

treatment_shift <- tibble(tibble(HF_research_data_2021_fscores) |>
                            dplyr::select(anon_tx_id) |>
                            distinct()) |>
  mutate(tx_shift = runif(n(),
                          min = -treatment_shift_days * 86400,
                          max =  treatment_shift_days * 86400))

## apply the shift ------------------------------------------------------------

# Join the per-respondent and per-treatment shifts back onto every row so the
# combined offset is looked up row-wise (no recycling across different lengths).
HF_research_data_2021_fscores <- HF_research_data_2021_fscores |>
  left_join(respondent_shift, by = "anon_id") |>
  left_join(treatment_shift,  by = "anon_tx_id") |>
  mutate(combined_shift = resp_shift + tx_shift)

# Shift every source timestamp by the row-wise combined offset, then replace the
# time-of-day. We split a timestamp into its calendar-day part and its
# time-of-day, keep the day, and re-attach a time-of-day drawn uniformly from
# [0, keep_time_of_day) of a day after midnight. With keep_time_of_day = 0 the
# real time-of-day is fully destroyed; ordering across days is unaffected.
for (col in source_dt) {
  x <- HF_research_data_2021_fscores[[col]] + HF_research_data_2021_fscores$combined_shift
  day_part <- as.POSIXct(lubridate::as_date(x), tz = "UTC")
   # keep_time_of_day = 0 destroys the real time-of-day by replacing it with a
   # uniform-random time; otherwise keep a fraction of the original time-of-day.
  new_tod <- if (keep_time_of_day == 0) {
    runif(length(x), 0, 86400)
  } else {
    frac <- x %% 86400
    runif(length(x), 0, keep_time_of_day * 86400) +
      keep_time_of_day * frac * (1 - keep_time_of_day)
   }
  new_tod[is.na(x)] <- NA
  HF_research_data_2021_fscores[[col]] <- as.POSIXct(day_part + new_tod, tz = "UTC")
}

# Recompute the derived / convenience timestamps from the shifted `date` so the
# within-person ordering (and the "first" values it drives) is preserved.
HF_research_data_2021_fscores <- HF_research_data_2021_fscores |>
  arrange(anon_id, date) |>
  group_by(anon_id) |>
  mutate(pt_first_date = min(date, na.rm = TRUE)) |>
  group_by(anon_id, anon_tx_id) |>
  mutate(tx_first_date = min(date, na.rm = TRUE)) |>
  ungroup()

# Drop the helper shift columns.
HF_research_data_2021_fscores <- HF_research_data_2021_fscores |>
  dplyr::select(-c(resp_shift, tx_shift, combined_shift))

## integrity checks -----------------------------------------------------------

# The "first" values must still equal the minimum date within each group.
pt_check <- HF_research_data_2021_fscores |>
  group_by(anon_id) |>
  summarise(ok_pt = all(pt_first_date == min(date, na.rm = TRUE),
                        na.rm = TRUE), .groups = "drop") |>
  dplyr::summarise(all = all(ok_pt))
print(pt_check)
stopifnot(pt_check$all == TRUE)

# The weeks-since-first columns are differences of dates that now carry the
# per-treatment shift, so the stored values no longer match the shifted dates.
# Recompute them from the shifted `date` so the published dataset is internally
# consistent.
if ("pt_wks_since_first" %in% names(HF_research_data_2021_fscores)) {
  HF_research_data_2021_fscores <- HF_research_data_2021_fscores |>
    group_by(anon_id) |>
    mutate(pt_wks_since_first = as.numeric(difftime(date,
                                                    min(date, na.rm = TRUE),
                                                    units = "weeks"))) |>
    ungroup()
}

if ("tx_wks_since_first" %in% names(HF_research_data_2021_fscores)) {
  HF_research_data_2021_fscores <- HF_research_data_2021_fscores |>
    group_by(anon_id, anon_tx_id) |>
    mutate(tx_wks_since_first = as.numeric(difftime(date,
                                                    min(date, na.rm = TRUE),
                                                    units = "weeks"))) |>
    ungroup()
  message("pt/tx_wks_since_first recomputed from shifted dates: OK")
}

# Sanity: timestamps should now sit far from the original real-time window.
cat("shifted `date` range: ",
    format(min(HF_research_data_2021_fscores$date, na.rm = TRUE)),
      " to ",
    format(max(HF_research_data_2021_fscores$date, na.rm = TRUE)), "\n")

## ---------------------------------------------------------------------------

usethis::use_data(HF_research_data_2021_fscores, overwrite = TRUE)
