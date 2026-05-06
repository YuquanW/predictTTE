test_that("assessment windows map latent event date to next assessment", {
  rand <- as.Date("2025-01-01")

  out1 <- predictTTE:::next_assessment_date(
    rand_date = rand,
    latent_event_date = as.Date("2025-02-20"),
    assessment_intervals_days = c(42, 84),
    assessment_cut_days = c(365.25)
  )
  expect_equal(out1, as.Date("2025-03-26"))

  out2 <- predictTTE:::next_assessment_date(
    rand_date = rand,
    latent_event_date = as.Date("2026-03-01"),
    assessment_intervals_days = c(42, 84),
    assessment_cut_days = c(365.25)
  )
  expect_true(out2 >= as.Date("2026-03-01"))
})

test_that("assessment windows can be disabled with NULL inputs", {
  rand <- c(10, 20)
  latent <- c(15.5, 30.25)

  out <- predictTTE:::adjust_event_times(
    rand_time = rand,
    latent_event_time = latent,
    assessment_intervals_days = NULL,
    assessment_cut_days = NULL
  )

  expect_equal(out, latent)
})

test_that("simulate_parametric_tte avoids NA draws for extreme exponential lp", {
  skip_if_not_installed("survival")

  set.seed(1)
  df <- data.frame(
    time = stats::rexp(200, rate = 0.02),
    status = rep(1L, 200),
    x = stats::rnorm(200)
  )
  fit <- survival::survreg(survival::Surv(time, status) ~ x, data = df, dist = "exponential")

  draws <- expect_warning(
    predictTTE:::simulate_parametric_tte(fit, data.frame(x = c(1e8, -1e8, 0))),
    NA
  )

  expect_false(anyNA(draws))
  expect_true(all(is.finite(draws)))
  expect_true(all(draws > 0))
})
