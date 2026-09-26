# NorseResearch 0.2.1

## Reverse-scoring audit

* Explain an `NA` audit result in the warning itself, naming each unresolved
  group and scale and whether the cause is insufficient overlap, no variation,
  weak correlations, intervals crossing zero, or sensitivity to `-98`. Expose
  the same concise explanations in the verbose `issues` table.
* Calculate audit scale means from one observed item by default so sparse and
  trigger-driven NF rows remain usable. Add `min_items` and retain optional
  `min_fraction` for analyses that require stricter scale completeness.

## QOL correction

* Validate QOL item Q226 in both the audit and scoring pipeline on its actual
  0--10 response scale rather than applying the general 1--7 NF item range.
  Q226 remains unreversed, is excluded from the audit's problem-direction
  verdict, and treats both `-98` and `-99` as missing during scoring.

# NorseResearch 0.2.0

This release clarifies the NF scoring contract. Main symptom and resource
scales are prepared so that higher scores mean more problems, regardless of
whether an export contains original agreement responses or responses that were
already standardized to that direction.

## Scoring direction and audit

* Add `audit_nf_reverse()` to diagnose the response direction of NF3 exports
  before scoring. Its default result is a single dataset-level verdict:
  `"consistent"`, `"not consistent"`, or `"mixed"`; insufficient or unresolved
  evidence returns `NA` rather than guessing.
* Add `verbose = TRUE` audit output with scale and item classifications,
  correlations, sentinel counts, proposed actions, item conflicts, mappings,
  and export-group summaries. The audit can select one observation per patient
  and examine batches or export sources separately.
* Compare audit conclusions both without `-98` and with `-98` interpreted as no
  problems. Treat `-99` as missing throughout the audit and report conclusions
  that depend on the `-98` sensitivity analysis.
* Exclude Alliance, Therapy Preferences/Needs, QOL, and Norse items from the
  problem-direction verdict because these items retain their exported response
  direction under the scoring policy.

## Clarified scoring API

* Add `prepare_nf_items()` as the shared item-preparation API. It accepts an
  explicit `input_coding` of `"higher_is_worse"` or `"agreement"`, including a
  row-level vector for reviewed export batches, plus optional reviewed
  `item_coding` overrides.
* Add the same `input_coding` and `item_coding` contract to `score_all()`,
  `score_all_nf3()`, `score_all_NORSE2()`, and `score_all_NORSE2_ou()`.
  Agreement-coded positive items are reversed exactly once before scales are
  calculated; already standardized items are kept unchanged.
* Score the main symptom and resource scales in a common higher = more problems
  direction. Resource-scale names therefore represent deficits when interpreted
  as problem-oriented scores.
* Keep Alliance, Therapy Preferences/Needs, QOL, and Norse (Q148, including
  Q148.2) unreversed. For these exceptions, both `-98` and `-99` are missing;
  they retain their original meaning and are not problem-severity scores.
* On the main problem-oriented scales, interpret `-98` as 1 (no problems) and
  `-99` as missing. Sentinel handling occurs before ordinary-response reversal,
  preventing `-98` from becoming a maximum-problem response.
* Preserve the caller's original item columns while scoring an internally
  prepared copy. `prepare_nf_items()` remains available when standardized item
  columns are needed directly.
* Make NF2, NF3, mixed-version, mean, trigger, and over/under scoring use the
  same preparation rules. NF3 scores ignore NF2-only items in combined exports,
  and QOL now follows the shared sentinel policy.
* Make low-level mean, trigger, over/under, and reversal helpers reject
  unprepared special codes or values outside 1--7. Clarify that norm tables
  must match the score direction, NF version, and scoring policy.

## Export handling and validation

* Add `has_no_98s_99s()` to detect `-98` and `-99` sentinel values in numeric
  columns. It can validate a data-frame pipeline, return a logical result, or
  warn while returning the input unchanged.
* Add `check_98s_99s` to the legacy `check_rev()` helper. Document
  `replace_98s_99s()` as a generic replacement utility rather than the NF
  scoring-preparation API.
* Document one recommended path from an NF3.1 export through cleaning, audit,
  explicit coding selection, and `score_all()`.

## Maintenance

* Expand regression coverage so equivalent agreement-coded and already
  standardized exports produce the same scale scores, including sentinel and
  scoring-exception cases.
* Enable real password-protected Excel import tests by initializing Reticulate
  correctly and adding encrypted `.xlsx` fixtures.

# NorseResearch 0.1.3

* Add `collapse_versioned_columns()` for deterministic numeric-suffix
  collapsing, with explicit conflict and type-coercion policies.
* Make suffix and paired-Q helpers delegate to the shared collapse primitive;
  add `get_first_nonmissing_obs()`, `check_nf_range()`, and `reverse_items()`.
* Normalize scoring version labels, use exact version matching, and centralize
  range and reverse-item validation.
* Fix ID resolution for names containing spaces and prevent duplicate target
  columns when translating Norwegian export aliases.

# NorseResearch 0.1.2

## Maintenance

* Make package functions and scoring workflows work when NorseResearch is used
  through `requireNamespace()` by bundling internal scoring and lookup data
  in the package namespace.
* Make `fix_failed_encoding()` conservative for ambiguous text and invalid
  UTF-8, and add coverage for mojibake patterns, clean text, invalid bytes,
  and idempotent repairs.
* Repair package examples and documentation so the source package builds and
  checks without errors or warnings.

# NorseResearch 0.1.1

## Maintenance

* Remove the `tidyverse` dependency from `Depends` and declare the package
  imports explicitly, avoiding namespace pollution when NorseResearch is
  attached.
* Keep the anonymized `HF_research_data_2021_fscores` data write as the single
  authoritative data-generation step.
* Update vignette rendering for current Pandoc versions.

# NorseResearch 0.1.0

This is the first public release of NorseResearch.

## New features

* Read and clean Norse Feedback NF2 and NF3 exports.
* Score NF2, NF3, and mixed-version data with raw scale scores.
* Add the cross-version Overall Negative Affect (`ona`) score. It uses the
  canonical 26-item definition; standard NF3.1 data are scored from the 23
  available items because `Q3`, `Q38`, and `Q141` are not in the NF3.1 item bank.
* Inspect Norse Feedback item metadata, triggers, and scale names.
* Analyze scale reliability and item response theory summaries.
* Plot scale, item, trigger, item-characteristic, and information functions.
* Generate synthetic Norse Feedback data for examples and testing.
