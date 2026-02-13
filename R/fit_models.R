#' Fit Enrollment, Event, and Dropout Models
#'
#' @param data ADTTE data with required columns.
#' @param enrollment_model Distribution for enrollment time.
#' @param event_model Distribution for event time.
#' @param dropout_model Distribution for dropout time.
#' @param covariates Optional covariates for event/dropout models.
#' @return A list of fitted `survreg` models.
#' @export
fit_tte_models <- function(data,
                           enrollment_model = "exponential",
                           event_model = "exponential",
                           dropout_model = "exponential",
                           covariates = NULL) {
  if (!requireNamespace("survival", quietly = TRUE)) {
    stop("Package 'survival' is required for fit_tte_models().", call. = FALSE)
  }

  validate_adtte_columns(data, covariates = covariates)

  enrollment_dist <- standardize_dist(enrollment_model)
  event_dist <- standardize_dist(event_model)
  dropout_dist <- standardize_dist(dropout_model)

  arrival_days <- pmax(as.numeric(data$randdt - data$trialsdt), 1e-8)
  enrollment_data <- data.frame(arrival_days = arrival_days)

  enrollment_fit <- survival::survreg(
    survival::Surv(arrival_days, rep(1, nrow(enrollment_data))) ~ 1,
    data = enrollment_data,
    dist = enrollment_dist
  )

  rhs <- if (is.null(covariates) || length(covariates) == 0L) {
    "1"
  } else {
    paste(covariates, collapse = " + ")
  }

  fit_data <- data
  fit_data$aval <- pmax(fit_data$aval, 1e-8)

  event_formula <- stats::as.formula(paste0("survival::Surv(aval, status == 1) ~ ", rhs))
  dropout_formula <- stats::as.formula(paste0("survival::Surv(aval, status == -1) ~ ", rhs))

  event_fit <- survival::survreg(event_formula, data = fit_data, dist = event_dist)
  dropout_fit <- survival::survreg(dropout_formula, data = fit_data, dist = dropout_dist)

  out <- list(
    enrollment_fit = enrollment_fit,
    event_fit = event_fit,
    dropout_fit = dropout_fit,
    covariates = covariates %||% character(0),
    distributions = list(
      enrollment = enrollment_dist,
      event = event_dist,
      dropout = dropout_dist
    )
  )
  class(out) <- c("predictTTE_fits", class(out))
  out
}
