# predictTTE

`predictTTE` predicts time-to-event milestones from interim ADTTE data.

## What it does

- Cleans EDC workbook data into a standard ADTTE structure.
- Fits parametric enrollment, event, and dropout models.
- Simulates future event trajectories with tumor assessment windows.
- Returns:
  - predicted dates (with confidence intervals) for target event counts;
  - predicted event counts (with confidence intervals) for target dates;
  - an interactive cumulative event plot with prediction-start and cutoff markers;
  - a CI-ribbon plot for target-date event count predictions.

## Install

```r
if (!requireNamespace("devtools", quietly = TRUE)) {
    install.packages("devtools")
}
devtools::install_github("YuquanW/predictTTE")
```

## Minimal usage

```r
library(predictTTE)

# Use packaged auto-loaded example data (.rda)
adtte <- example_adtte_pfs

res <- predict_tte(
  data = adtte,
  planned_total_n = 176,
  target_events = c(120, 140, 160),
  target_dates = as.Date(c("2026-02-01", "2026-05-01", "2026-08-01")),
  nsim = 1000,
  assessment_intervals_days = c(42, 84),
  assessment_cut_days = c(365.25),
  enrollment_model = "exponential",
  event_model = "exponential",
  dropout_model = "exponential",
  ci_level = 0.95,
  fixed_parameters = TRUE,
  seed = 123
)

res$pred_dates
res$pred_event_counts
res$plot
res$pred_event_plot

# one of target_events / target_dates can be NULL
# (but not both NULL)
```

## Project structure

- `R/`: package functions
- `data/`: example ADTTE dataset (`.rda`, auto-loaded)
- `inst/extdata/`: anonymized reference CSV files
- `tests/testthat/`: unit tests
- `vignettes/`: long-form package guide
