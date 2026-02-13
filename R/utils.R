`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

as_date_safe <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }

  if (is.numeric(x)) {
    return(as.Date(x, origin = "1899-12-30"))
  }

  x_chr <- as.character(x)
  suppressWarnings(x_num <- as.numeric(x_chr))
  out <- as.Date(x_chr)

  needs_excel_parse <- is.na(out) & !is.na(x_num)
  out[needs_excel_parse] <- as.Date(x_num[needs_excel_parse], origin = "1899-12-30")
  out
}

standardize_dist <- function(model) {
  key <- tolower(gsub("[-_[:space:]]", "", model))
  mapped <- switch(
    key,
    exp = "exponential",
    exponential = "exponential",
    weibull = "weibull",
    lnorm = "lognormal",
    lognormal = "lognormal",
    "log-normal" = "lognormal",
    llogis = "loglogistic",
    loglogistic = "loglogistic",
    "log-logistic" = "loglogistic",
    model
  )

  if (!mapped %in% c("exponential", "weibull", "lognormal", "loglogistic")) {
    stop(
      "Only exponential, weibull, log-normal, and log-logistic are currently supported. ",
      "Got: ", model,
      call. = FALSE
    )
  }

  mapped
}

validate_assessment_windows <- function(assessment_intervals_days, assessment_cut_days) {
  if (is.null(assessment_intervals_days) && is.null(assessment_cut_days)) {
    return(invisible(TRUE))
  }

  if (is.null(assessment_intervals_days) && !is.null(assessment_cut_days)) {
    stop("assessment_cut_days cannot be provided when assessment_intervals_days is NULL.", call. = FALSE)
  }

  if (is.null(assessment_cut_days)) {
    if (length(assessment_intervals_days) == 1L) {
      assessment_cut_days <- numeric(0)
    } else {
      stop(
        "assessment_cut_days must be provided when assessment_intervals_days has length > 1.",
        call. = FALSE
      )
    }
  }

  if (length(assessment_intervals_days) < 1L) {
    stop("assessment_intervals_days must contain at least one value.", call. = FALSE)
  }

  if (any(!is.finite(assessment_intervals_days)) || any(assessment_intervals_days <= 0)) {
    stop("assessment_intervals_days must be positive finite numbers.", call. = FALSE)
  }

  k <- length(assessment_intervals_days)
  if (length(assessment_cut_days) != (k - 1L)) {
    stop(
      "assessment_cut_days must have length length(assessment_intervals_days) - 1.",
      call. = FALSE
    )
  }

  if (length(assessment_cut_days) > 0L && is.unsorted(assessment_cut_days, strictly = TRUE)) {
    stop("assessment_cut_days must be strictly increasing.", call. = FALSE)
  }

  invisible(TRUE)
}

normalize_assessment_windows <- function(assessment_intervals_days, assessment_cut_days) {
  if (is.null(assessment_intervals_days) && is.null(assessment_cut_days)) {
    return(list(enabled = FALSE, intervals = NULL, cuts = NULL))
  }

  if (is.null(assessment_cut_days)) {
    assessment_cut_days <- if (length(assessment_intervals_days) == 1L) numeric(0) else assessment_cut_days
  }

  validate_assessment_windows(assessment_intervals_days, assessment_cut_days)
  list(
    enabled = TRUE,
    intervals = as.numeric(assessment_intervals_days),
    cuts = as.numeric(assessment_cut_days)
  )
}

validate_adtte_columns <- function(data, covariates = NULL) {
  required <- c("usubjid", "trialsdt", "randdt", "adt", "cutoffdt", "status", "aval")
  missing <- setdiff(required, names(data))

  if (length(missing) > 0L) {
    stop("data is missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  if (!all(data$status %in% c(-1, 0, 1))) {
    stop("status must only contain -1 (dropout), 0 (censored), or 1 (event).", call. = FALSE)
  }

  key_cols <- c("usubjid", "trialsdt", "randdt", "adt", "cutoffdt", "status", "aval")
  if (any(!stats::complete.cases(data[key_cols]))) {
    stop("Required ADTTE columns contain missing values.", call. = FALSE)
  }

  if (any(data$aval < 0, na.rm = TRUE)) {
    stop("aval must be non-negative.", call. = FALSE)
  }

  if (!is.null(covariates)) {
    cov_missing <- setdiff(covariates, names(data))
    if (length(cov_missing) > 0L) {
      stop("covariates missing from data: ", paste(cov_missing, collapse = ", "), call. = FALSE)
    }
  }

  invisible(TRUE)
}

adjust_event_times <- function(rand_time,
                               latent_event_time,
                               assessment_intervals_days = NULL,
                               assessment_cut_days = NULL) {
  windows <- normalize_assessment_windows(assessment_intervals_days, assessment_cut_days)

  rand_time <- as.numeric(rand_time)
  latent_event_time <- as.numeric(latent_event_time)

  if (!windows$enabled) {
    return(latent_event_time)
  }

  intervals <- windows$intervals
  cuts <- windows$cuts
  k <- length(intervals)

  piece_start <- numeric(k)
  piece_end <- rep(Inf, k)
  if (k > 1L) {
    current_start <- 0
    for (i in seq_len(k - 1L)) {
      n_piece <- max(0, ceiling((cuts[i] - current_start) / intervals[i]))
      current_start <- current_start + n_piece * intervals[i]
      piece_end[i] <- current_start
      piece_start[i + 1L] <- current_start
    }
  }

  elapsed <- latent_event_time - rand_time
  out_elapsed <- elapsed

  idx_nonpos <- which(!is.na(elapsed) & elapsed <= 0)
  if (length(idx_nonpos) > 0L) {
    out_elapsed[idx_nonpos] <- 0
  }

  idx_pos <- which(!is.na(elapsed) & elapsed > 0)
  if (length(idx_pos) > 0L) {
    piece_idx <- findInterval(elapsed[idx_pos], vec = piece_end[seq_len(k - 1L)], left.open = FALSE) + 1L
    starts <- piece_start[piece_idx]
    steps <- intervals[piece_idx]
    out_elapsed[idx_pos] <- starts + ceiling((elapsed[idx_pos] - starts) / steps) * steps
  }

  rand_time + out_elapsed
}

next_assessment_date <- function(rand_date, latent_event_date, assessment_intervals_days, assessment_cut_days) {
  out <- adjust_event_times(
    rand_time = as.numeric(rand_date),
    latent_event_time = as.numeric(latent_event_date),
    assessment_intervals_days = assessment_intervals_days,
    assessment_cut_days = assessment_cut_days
  )
  as.Date(out, origin = "1970-01-01")
}

simulate_parametric_tte <- function(fit, newdata, fixed_parameters = TRUE) {
  if (!inherits(fit, "survreg")) {
    stop("fit must be a survreg model.", call. = FALSE)
  }

  mm <- stats::model.matrix(stats::delete.response(stats::terms(fit)), newdata)
  coefs <- stats::coef(fit)

  if (!fixed_parameters) {
    vc <- fit$var[seq_along(coefs), seq_along(coefs), drop = FALSE]
    if (!requireNamespace("MASS", quietly = TRUE)) {
      stop("MASS is required when fixed_parameters = FALSE.", call. = FALSE)
    }
    coefs <- as.numeric(MASS::mvrnorm(1L, mu = coefs, Sigma = vc))
  }

  lp <- as.numeric(mm %*% coefs)
  dist <- fit$dist
  scale <- fit$scale
  n <- nrow(mm)

  if (dist == "exponential") {
    rate <- exp(-lp)
    return(stats::rexp(n, rate = rate))
  }

  if (dist == "weibull") {
    shape <- 1 / scale
    return(stats::rweibull(n, shape = shape, scale = exp(lp)))
  }

  if (dist == "lognormal") {
    return(stats::rlnorm(n, meanlog = lp, sdlog = scale))
  }

  if (dist == "loglogistic") {
    shape <- 1 / scale
    u <- stats::runif(n)
    return(exp(lp) * (u / (1 - u))^(1 / shape))
  }

  stop("Unsupported fitted distribution in simulate_parametric_tte: ", dist, call. = FALSE)
}

simulate_conditional_tte <- function(fit, newdata, t0, fixed_parameters = TRUE) {
  t_draw <- simulate_parametric_tte(fit, newdata, fixed_parameters = fixed_parameters)

  # Rejection sampling is simple and robust for conditional draws T | T > t0.
  while (any(t_draw <= t0, na.rm = TRUE)) {
    idx <- which(t_draw <= t0)
    t_draw[idx] <- simulate_parametric_tte(fit, newdata[idx, , drop = FALSE], fixed_parameters = fixed_parameters)
  }

  t_draw
}

summarize_ci <- function(x, ci_level) {
  alpha <- (1 - ci_level) / 2
  stats::quantile(x, probs = c(alpha, 0.5, 1 - alpha), na.rm = TRUE, names = FALSE)
}
