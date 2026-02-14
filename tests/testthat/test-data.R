test_that("example_adtte dataset is available", {
  data("example_adtte", package = "predictTTE", envir = environment())
  expect_true(exists("example_adtte", envir = environment(), inherits = FALSE))

  d <- get("example_adtte", envir = environment())
  expect_true(is.data.frame(d))
  expect_true(all(c("usubjid", "trialsdt", "randdt", "adt", "cutoffdt", "status", "aval") %in% names(d)))
  expect_true(all(grepl("^PT-[0-9]{4}$", d$usubjid)))
  expect_false("eventdscrp" %in% names(d))
  expect_false("terminate" %in% names(d))
  expect_false("endpoint" %in% names(d))
})
