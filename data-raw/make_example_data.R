# Regenerate anonymized example ADTTE data assets.
# Run this script from package root.

library(readr)

example_adtte <- readr::read_csv(
  "inst/extdata/example_adtte.csv",
  show_col_types = FALSE
)

example_adtte <- as.data.frame(example_adtte)
example_adtte <- example_adtte[seq_len(min(300L, nrow(example_adtte))), , drop = FALSE]
example_adtte$trialsdt <- as.Date(example_adtte$trialsdt)
example_adtte$randdt <- as.Date(example_adtte$randdt)
example_adtte$adt <- as.Date(example_adtte$adt)
example_adtte$cutoffdt <- as.Date(example_adtte$cutoffdt)

save(example_adtte, file = "data/example_adtte.rda", compress = "xz")
