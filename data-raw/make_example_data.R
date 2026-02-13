# Regenerate anonymized example ADTTE data assets.
# Run this script from package root.

library(readr)

example_adtte_pfs <- readr::read_csv(
  "inst/extdata/example_adtte_pfs.csv",
  show_col_types = FALSE
)

example_adtte_pfs <- as.data.frame(example_adtte_pfs)
example_adtte_pfs$trialsdt <- as.Date(example_adtte_pfs$trialsdt)
example_adtte_pfs$randdt <- as.Date(example_adtte_pfs$randdt)
example_adtte_pfs$adt <- as.Date(example_adtte_pfs$adt)
example_adtte_pfs$cutoffdt <- as.Date(example_adtte_pfs$cutoffdt)

save(example_adtte_pfs, file = "data/example_adtte_pfs.rda", compress = "xz")
