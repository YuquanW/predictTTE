test_that("example_adtte_pfs dataset is available", {
  data("example_adtte_pfs", package = "predictTTE", envir = environment())
  expect_true(exists("example_adtte_pfs", envir = environment(), inherits = FALSE))

  d <- get("example_adtte_pfs", envir = environment())
  expect_true(is.data.frame(d))
  expect_true(all(c("usubjid", "trialsdt", "randdt", "adt", "cutoffdt", "status", "aval") %in% names(d)))
  expect_true(all(grepl("^PT-[0-9]{4}$", d$usubjid)))
  expect_false("eventdscrp" %in% names(d))
  expect_false("terminate" %in% names(d))
})
