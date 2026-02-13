#' Run a Minimal End-to-End Example
#'
#' Demonstrates how to use packaged ADTTE input with `predict_tte()`.
#'
#' @param nsim Number of simulations.
#' @return A `predictTTE_result` object.
#' @export
example_predict_tte <- function(nsim = 200) {
  if (!exists("example_adtte_pfs", inherits = TRUE)) {
    data("example_adtte_pfs", package = "predictTTE", envir = environment())
  }
  data <- get("example_adtte_pfs", envir = environment())

  predict_tte(
    data = data,
    planned_total_n = 176,
    target_events = c(120, 140, 160),
    target_dates = as.Date(c("2026-02-01", "2026-05-01", "2026-08-01")),
    nsim = nsim,
    assessment_intervals_days = c(42, 84),
    assessment_cut_days = c(365.25),
    enrollment_model = "exponential",
    event_model = "exponential",
    dropout_model = "exponential",
    ci_level = 0.95,
    covariates = NULL,
    fixed_parameters = TRUE,
    seed = 123
  )
}
