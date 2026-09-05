# NorseResearch

`NorseResearch` provides functions and documentation for working with data
exported from **Norse Feedback (NF)**, the Norwegian web-based outcome-measure
system used in mental-health and substance-use treatment. The package helps you
read NF exports, clean and reshape them, score the standard scales, and
inspect or plot the resulting items and scales.

The package contains the code and item documentation, but **not** the patient
data itself, which is confidential. A small set of **synthetic** data is bundled
so you can run the examples without needing a real export.

## Installation

The package is not on CRAN. Install the development version from GitHub:

```r
# install.packages("devtools")    # first-time setup only
devtools::install_github("andrewmcaleavey/NorseResearch")
```

## Getting started

```r
library(NorseResearch)
library(tidyverse)

# Synthetic (random) data, similar in shape to a real NF export.
data(synthetic_data)
synthetic_data
```

Most functions operate on a data frame whose item columns are named like `Q42`,
`Q102`, and so on. This is not the format of most raw NF data that has been 
exported - a cleaning pipeline needs to be applied to raw data first, in order 
to ensure proper variable names and variable similarity.  

The bundled `synthetic_data` is reproducible effectively random NF2 data; it 
has trigger-driven `NA` values for scales that were not administered and 
pre-computed scales with names like `sad` and `cog`. It is a quick way to try 
the analysis and plotting functions even though it will not generate plausible 
results.  

## Key functions by task

The [Introduction vignette](vignettes/introduction.Rmd) walks through a full
pipeline. The tables below summarize the main functions. Each name links to its
help page in R: use `?function_name` (for example, `?scale_analysis2`), or use
`?` with the name and let autocomplete fill it in.

### Read and repair imports

| Function | What it does |
|---|---|
| [`read.csv_nf3()`](?read.csv_nf3) | Read a comma-delimited NF3 export; treats `-99` as `NA`. |
| [`read.csv2_nf3()`](?read.csv2_nf3) | Read a semicolon-delimited / European-decimal NF3 export. |
| [`read_excel_nf3()`](?read_excel_nf3) | Read an `.xlsx` NF3 export via `readxl`. |
| [`read_password_protected_excel()`](?read_password_protected_excel) | Decrypt and read password-protected Excel files (via `reticulate`). |
| [`fix_failed_encoding()`](?fix_failed_encoding) | Repair likely mojibake in character columns. |

### Clean and reshape

| Function | What it does |
|---|---|
| [`clean_NF_names()`](?clean_NF_names) | Standardize messy export column names. |
| [`collapse_measures_wide()`](?collapse_measures_wide) | Collapse `_A#` repeated-measure columns and widen multi-measure items. |
| [`combine_suffix_variables()`](?combine_suffix_variables), [`combine_q_vars()`](?combine_q_vars) | Merge related columns (by suffix or `Q`-pattern). |
| [`replace_98s_99s()`](?replace_98s_99s) | Replace sentinel `-98`/`-99` with `1`/`NA`. |
| [`nicer_id_var()`](?nicer_id_var) | Build a readable respondent ID column. |
| [`get_first_obs()`](?get_first_obs) | Keep the first observation per respondent. |

### Versioning, lookup, and metadata

| Function | What it does |
|---|---|
| [`check_version_nf()`](?check_version_nf) | Detect which NF version(s) (`2`, `3`, or both) are in a data set. |
| [`lookup_item()`](?lookup_item) | Look up an item's text by `Q`-number. |
| [`get_item_text()`](?get_item_text) | Deprecated alias for [`lookup_item()`](?lookup_item). |
| [`get_nicer_name()`](?get_nicer_name), [`get_nf3_nicer_name()`](?get_nf3_nicer_name) | Convert a short scale name (e.g. `"sad"`) to a readable one (e.g. `"Sad Affect"`). |
| [`lookup_trigger()`](?lookup_trigger), [`lookup_trigger_among()`](?lookup_trigger_among) | Identify trigger items by scale name or within a list of items. |

### Score scales

| Function | What it does |
|---|---|
| [`score_all()`](?score_all) | Score all NF2 and/or NF3 scales in raw-scale form. |
| [`score_all_nf3()`](?score_all_nf3) | Score the NF3 scales specifically. |
| [`nf_score()`](?nf_score) | Deprecated mixed-version compatibility helper; use [`score_all()`](?score_all). |
| [`score_NORSE_trigger()`](?score_NORSE_trigger) | Score a scale while keeping subthreshold trigger responses. |
| [`score_normed_NF()`](?score_normed_NF) | Compute normed scores from fixed norms. |
| [`rename_score_vars()`](?rename_score_vars) | Map exported `SCORE_*` variables to consistent names, for comparison only. |

> `score_all()` and the `score_all_nf3()` family expect a data set whose item
> columns are already cleaned to short `Q#` names. They are built around real NF
> exports.

### Analyze and plot

| Function | What it does |
|---|---|
| [`scale_analysis2()`](?scale_analysis2) | Reliability (omega, alpha) and IRT summaries for a scale. Use this; [`scale_analysis()`](?scale_analysis) is deprecated. |
| [`scale_plot()`](?scale_plot) | Annotated histogram of a scale-score distribution. |
| [`item_plot()`](?item_plot) | Histogram of a single item's responses. |
| [`trigger_plot()`](?trigger_plot) | Predicted distribution around a trigger item (needs a `mirt` fit). |
| [`ggplot_icc_plot()`](?ggplot_icc_plot), [`ggplot_information_plot()`](?ggplot_information_plot) | Item-characteristic-curve and information-function plots for a `mirt`/`ltm` fit. |

### Generate mock NF data

| Function | What it does |
|---|---|
| [`random_norse_data()`](?random_norse_data) | Generate a mock NF data set of random responses. |
| [`random_person_generator()`](?random_person_generator), [`random_tx_generator()`](?random_tx_generator) | Build the person and treatment (observation) layers used above. |

## A quick example

```r
library(NorseResearch)
library(tidyverse)
data(synthetic_data)

# Which NF version is this?
check_version_nf(synthetic_data)           # "2"

# Readable name for a scale's short code
get_nf3_nicer_name("sad")                 # "Sad Affect"

# Reliability + IRT summary for the Sad Affect scale
sa <- scale_analysis2("Sad affect", sad.names, synthetic_data)
print(sa)
plot(sa)
```

### Generate a mixed-version mock export

`random_norse_data()` can produce NF2, NF3, or a mixed export. In mixed data,
`Ver_10` identifies the version and each patient's NF2 rows precede their NF3
rows. The optional sentinel values are raw-export values; `score_all()` handles
them automatically when scoring.

```r
set.seed(20260903)
mock <- random_norse_data(
  20,
  num_obs = 5,
  versions = c("2", "3"),
  include_98 = TRUE,
  include_99 = TRUE,
  sentinel_probability = 0.05
)

check_version_nf(mock) # c("2", "3")
scored_mock <- score_all(mock)
```

## Learn more

- The [Introduction vignette](vignettes/introduction.Rmd) — an end-to-end workflow.
- `?NorseResearch` — package overview and the list of bundled data objects.

## License

This package is licensed under the [MIT license](LICENSE.md).
