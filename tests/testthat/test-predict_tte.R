test_that("predict_tte returns the required outputs", {
  skip_if_not_installed("survival")
  skip_if_not_installed("plotly")
  skip_if_not_installed("tibble")

  n <- 40
  trialsdt <- as.Date("2024-01-01")
  randdt <- trialsdt + sample(0:120, n, replace = TRUE)
  aval <- sample(60:280, n, replace = TRUE)
  status <- sample(c(-1L, 0L, 1L), n, replace = TRUE, prob = c(0.15, 0.45, 0.40))
  status[1] <- 0L
  status[2] <- 1L
  status[3] <- -1L
  adt <- randdt + aval

  data <- data.frame(
    usubjid = sprintf("ID%03d", seq_len(n)),
    trialsdt = trialsdt,
    randdt = randdt,
    adt = adt,
    cutoffdt = as.Date("2025-01-01"),
    status = status,
    aval = aval,
    stringsAsFactors = FALSE
  )

  res <- predict_tte(
    data = data,
    planned_total_n = n,
    target_events = c(5, 10),
    target_dates = as.Date(c("2025-03-01", "2025-06-01")),
    nsim = 50,
    assessment_intervals_days = c(42, 84),
    assessment_cut_days = c(365.25),
    enrollment_model = "exponential",
    event_model = "exponential",
    dropout_model = "exponential",
    ci_level = 0.9,
    fixed_parameters = TRUE,
    seed = 101
  )

  expect_true(is.list(res))
  expect_true(inherits(res, "PredRes"))
  expect_true(all(c("pred_dates", "pred_event_counts", "plot", "pred_event_plot", "meta") %in% names(res)))
  expect_equal(nrow(res$pred_dates), 2)
  expect_equal(nrow(res$pred_event_counts), 2)
  expect_equal(
    res$meta$prediction_start_date,
    min(data$adt[data$status == 0L])
  )
})

test_that("log-normal model is accepted", {
  skip_if_not_installed("survival")

  n <- 30
  trialsdt <- as.Date("2024-01-01")
  randdt <- trialsdt + sample(0:30, n, replace = TRUE)
  aval <- sample(10:120, n, replace = TRUE)
  status <- sample(c(-1L, 0L, 1L), n, replace = TRUE)
  status[1] <- 0L
  status[2] <- 1L
  status[3] <- -1L
  adt <- randdt + aval

  data <- data.frame(
    usubjid = sprintf("ID%03d", seq_len(n)),
    trialsdt = trialsdt,
    randdt = randdt,
    adt = adt,
    cutoffdt = as.Date("2025-01-01"),
    status = status,
    aval = aval,
    stringsAsFactors = FALSE
  )

  fit <- fit_tte_models(
    data = data,
    enrollment_model = "log-normal",
    event_model = "log-normal",
    dropout_model = "log-normal"
  )

  expect_true(inherits(fit$event_fit, "survreg"))
})

test_that("print/showDefault on PredRes only show formatted prediction summaries", {
  skip_if_not_installed("survival")
  skip_if_not_installed("plotly")
  skip_if_not_installed("tibble")

  n <- 20
  trialsdt <- as.Date("2024-01-01")
  randdt <- trialsdt + sample(0:30, n, replace = TRUE)
  aval <- sample(30:120, n, replace = TRUE)
  status <- sample(c(-1L, 0L, 1L), n, replace = TRUE)
  status[1] <- 0L
  status[2] <- 1L
  status[3] <- -1L
  adt <- randdt + aval

  data <- data.frame(
    usubjid = sprintf("ID%03d", seq_len(n)),
    trialsdt = trialsdt,
    randdt = randdt,
    adt = adt,
    cutoffdt = as.Date("2025-01-01"),
    status = status,
    aval = aval,
    stringsAsFactors = FALSE
  )

  res <- predict_tte(
    data = data,
    planned_total_n = n,
    target_events = c(5L),
    target_dates = as.Date(c("2025-03-01")),
    nsim = 10,
    seed = 1,
    show_progress = FALSE
  )

  expect_output(print(res), "Predicted dates for targeted event numbers")
  expect_output(showDefault(res), "Predicted event counts for targeted dates")
  expect_s3_class(res, "PredRes")
  expect_s3_class(plot(res), "PredRes")
})

test_that("predict_tte allows target_dates = NULL", {
  skip_if_not_installed("survival")
  skip_if_not_installed("plotly")
  skip_if_not_installed("tibble")

  n <- 25
  trialsdt <- as.Date("2024-01-01")
  randdt <- trialsdt + sample(0:80, n, replace = TRUE)
  aval <- sample(30:160, n, replace = TRUE)
  status <- sample(c(-1L, 0L, 1L), n, replace = TRUE)
  status[1] <- 0L
  status[2] <- 1L
  status[3] <- -1L
  adt <- randdt + aval

  data <- data.frame(
    usubjid = sprintf("ID%03d", seq_len(n)),
    trialsdt = trialsdt,
    randdt = randdt,
    adt = adt,
    cutoffdt = as.Date("2025-01-01"),
    status = status,
    aval = aval,
    stringsAsFactors = FALSE
  )

  res <- predict_tte(
    data = data,
    planned_total_n = n,
    target_events = c(5, 8),
    target_dates = NULL,
    nsim = 20,
    seed = 1
  )

  expect_true(is.data.frame(res$pred_dates))
  expect_null(res$pred_event_counts)
  expect_null(res$pred_event_plot)
})

test_that("predict_tte allows target_events = NULL", {
  skip_if_not_installed("survival")
  skip_if_not_installed("plotly")
  skip_if_not_installed("tibble")

  n <- 25
  trialsdt <- as.Date("2024-01-01")
  randdt <- trialsdt + sample(0:80, n, replace = TRUE)
  aval <- sample(30:160, n, replace = TRUE)
  status <- sample(c(-1L, 0L, 1L), n, replace = TRUE)
  status[1] <- 0L
  status[2] <- 1L
  status[3] <- -1L
  adt <- randdt + aval

  data <- data.frame(
    usubjid = sprintf("ID%03d", seq_len(n)),
    trialsdt = trialsdt,
    randdt = randdt,
    adt = adt,
    cutoffdt = as.Date("2025-01-01"),
    status = status,
    aval = aval,
    stringsAsFactors = FALSE
  )

  res <- predict_tte(
    data = data,
    planned_total_n = n,
    target_events = NULL,
    target_dates = as.Date(c("2025-03-01", "2025-04-01")),
    nsim = 20,
    seed = 1
  )

  expect_null(res$pred_dates)
  expect_true(is.data.frame(res$pred_event_counts))
  expect_true(!is.null(res$pred_event_plot))
})

test_that("predict_tte errors when both target inputs are NULL", {
  skip_if_not_installed("survival")
  skip_if_not_installed("plotly")
  skip_if_not_installed("tibble")

  data <- data.frame(
    usubjid = c("ID001", "ID002", "ID003"),
    trialsdt = as.Date("2024-01-01"),
    randdt = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
    adt = as.Date(c("2024-03-01", "2024-03-10", "2024-04-01")),
    cutoffdt = as.Date("2025-01-01"),
    status = c(0L, 1L, -1L),
    aval = c(60, 68, 89),
    stringsAsFactors = FALSE
  )

  expect_error(
    predict_tte(
      data = data,
      planned_total_n = 3,
      target_events = NULL,
      target_dates = NULL,
      nsim = 5
    ),
    "At least one of target_events or target_dates must be provided"
  )
})

test_that("target event prediction counts observed events through cutoff", {
  skip_if_not_installed("survival")
  skip_if_not_installed("plotly")
  skip_if_not_installed("tibble")

  data <- data.frame(
    usubjid = c("ID001", "ID002", "ID003", "ID004"),
    trialsdt = as.Date("2024-01-01"),
    randdt = as.Date(c("2024-01-01", "2024-01-05", "2024-01-10", "2024-01-12")),
    adt = as.Date(c("2024-02-01", "2024-03-01", "2024-04-01", "2024-02-15")),
    cutoffdt = as.Date("2024-05-01"),
    status = c(1L, 1L, -1L, -1L),
    aval = c(32, 57, 83, 35),
    stringsAsFactors = FALSE
  )

  res <- predict_tte(
    data = data,
    planned_total_n = nrow(data),
    target_events = c(2L),
    target_dates = NULL,
    nsim = 30,
    seed = 1
  )

  # With no ongoing subjects and no additional enrollment, the 2nd event is fixed.
  expect_equal(res$pred_dates$pred_date[1], as.Date("2024-03-01"))
  expect_equal(res$meta$observed_events_at_cutoff, 2)
})
