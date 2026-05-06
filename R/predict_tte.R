#' Predict Event Counts and Dates from ADTTE Data
#'
#' @param data ADTTE data with columns `usubjid`, `trialsdt`, `randdt`, `adt`,
#'   `cutoffdt`, `status`, `aval`, and optional covariates.
#' @param planned_total_n Planned total sample size.
#' @param target_events Integer vector of target event counts. Can be `NULL`
#'   if `target_dates` is provided.
#' @param target_dates Date vector of target prediction dates. Can be `NULL`
#'   if `target_events` is provided.
#' @param nsim Number of simulations.
#' @param assessment_intervals_days Piecewise tumor assessment intervals (days),
#'   length `k`. Can be `NULL` to disable adjustment.
#' @param assessment_cut_days Time points (days since randomization) where interval
#'   changes, length `k - 1`. Can be `NULL` when interval adjustment is disabled
#'   or when `assessment_intervals_days` has length 1.
#' @param enrollment_model Distribution for enrollment model.
#' @param event_model Distribution for event model.
#' @param dropout_model Distribution for dropout model.
#' @param fuyears Simulated follow-up years.
#' @param ci_level Confidence interval level.
#' @param covariates Optional covariates in event/dropout models.
#' @param fixed_parameters Use fitted point estimates (`TRUE`) or sample coefficients
#'   from asymptotic distribution (`FALSE`).
#' @param seed Optional seed for reproducible simulation.
#' @param show_progress Logical; if `TRUE`, show a simulation progress bar.
#' @param show_plot Logical; if `TRUE`, display the prediction plot after simulation.
#' @return A list with prediction tables and interactive plots.
#' @export
predict_tte <- function(data,
                        planned_total_n,
                        target_events = NULL,
                        target_dates = NULL,
                        nsim = 1000,
                        assessment_intervals_days = NULL,
                        assessment_cut_days = NULL,
                        enrollment_model = "exponential",
                        event_model = "exponential",
                        dropout_model = "exponential",
                        fuyears = 1,
                        ci_level = 0.95,
                        covariates = NULL,
                        fixed_parameters = TRUE,
                        seed = NULL,
                        show_progress = TRUE,
                        show_plot = FALSE) {
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("Package 'tibble' is required for predict_tte().", call. = FALSE)
  }
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Package 'plotly' is required for predict_tte().", call. = FALSE)
  }

  validate_adtte_columns(data, covariates = covariates)
  windows <- normalize_assessment_windows(assessment_intervals_days, assessment_cut_days)

  if (!is.null(seed)) {
    set.seed(seed)
  }

  has_target_events <- !is.null(target_events) && length(target_events) > 0L
  has_target_dates <- !is.null(target_dates) && length(target_dates) > 0L
  if (!has_target_events && !has_target_dates) {
    stop("At least one of target_events or target_dates must be provided.", call. = FALSE)
  }

  if (!is.numeric(planned_total_n) || length(planned_total_n) != 1L || planned_total_n <= 0) {
    stop("planned_total_n must be one positive number.", call. = FALSE)
  }

  if (!is.numeric(nsim) || length(nsim) != 1L || nsim <= 0) {
    stop("nsim must be one positive integer.", call. = FALSE)
  }

  if (has_target_events) {
    target_events <- as.integer(sort(unique(target_events)))
  } else {
    target_events <- integer(0)
  }

  if (has_target_dates) {
    target_dates <- sort(unique(as_date_safe(target_dates)))
    target_date_num <- as.numeric(target_dates)
  } else {
    target_dates <- as.Date(character(0))
    target_date_num <- numeric(0)
  }

  trialsdt <- min(as_date_safe(data$trialsdt), na.rm = TRUE)
  cutoff_date <- max(as_date_safe(data$cutoffdt), na.rm = TRUE)

  ongoing <- data[data$status == 0L, , drop = FALSE]
  pred_start <- if (nrow(ongoing) > 0L) {
    min(as_date_safe(ongoing$adt), na.rm = TRUE)
  } else {
    cutoff_date
  }

  event_dates_all <- as.numeric(as_date_safe(data$adt[data$status == 1L]))
  observed_events_at_cutoff <- sort(event_dates_all[event_dates_all <= as.numeric(cutoff_date)])
  observed_events_at_pred_start <- sort(event_dates_all[event_dates_all <= as.numeric(pred_start)])

  fits <- fit_tte_models(
    data = data,
    enrollment_model = enrollment_model,
    event_model = event_model,
    dropout_model = dropout_model,
    covariates = covariates
  )

  n_existing <- nrow(data)
  n_new <- max(0L, as.integer(planned_total_n) - n_existing)
  covars <- fits$covariates

  event_date_mat <- if (has_target_events) {
    matrix(NA_real_, nrow = nsim, ncol = length(target_events))
  } else {
    NULL
  }
  event_count_mat <- if (has_target_dates) {
    matrix(NA_real_, nrow = nsim, ncol = length(target_dates))
  } else {
    NULL
  }

  max_target_date <- if (has_target_dates) max(target_dates) else as.Date(NA)
  plot_end_date <- if (has_target_dates) max(max_target_date + 365*fuyears, cutoff_date + 365*fuyears) else (cutoff_date + 365*fuyears)
  plot_grid <- seq(trialsdt, plot_end_date, by = "7 days")
  plot_grid_num <- as.numeric(plot_grid)
  plot_mat <- matrix(NA_real_, nrow = nsim, ncol = length(plot_grid))

  cutoff_offset <- as.numeric(cutoff_date - trialsdt)
  trialsdt_num <- as.numeric(trialsdt)

  ongoing_rand_num <- as.numeric(as_date_safe(ongoing$randdt))
  ongoing_t0 <- ongoing$aval

  pb <- NULL
  if (isTRUE(show_progress)) {
    pb <- utils::txtProgressBar(min = 0, max = nsim, initial = 0, style = 3)
    on.exit(close(pb), add = TRUE)
  }

  for (s in seq_len(nsim)) {
    sim_events <- observed_events_at_cutoff

    if (nrow(ongoing) > 0L) {
      event_t <- simulate_conditional_tte(
        fit = fits$event_fit,
        newdata = ongoing,
        t0 = ongoing_t0,
        fixed_parameters = fixed_parameters
      )
      dropout_t <- simulate_conditional_tte(
        fit = fits$dropout_fit,
        newdata = ongoing,
        t0 = ongoing_t0,
        fixed_parameters = fixed_parameters
      )

      latent_event <- ongoing_rand_num + event_t - 1
      latent_dropout <- ongoing_rand_num + dropout_t - 1
      adjusted_event <- adjust_event_times(
        rand_time = ongoing_rand_num,
        latent_event_time = latent_event,
        assessment_intervals_days = windows$intervals,
        assessment_cut_days = windows$cuts
      )
      observed_now <- adjusted_event <= latent_dropout
      idx_now <- which(!is.na(observed_now) & observed_now)
      if (length(idx_now) > 0L) {
        sim_events <- c(sim_events, adjusted_event[idx_now])
      }
    }

    if (n_new > 0L) {
      enrollment_draw <- simulate_conditional_tte(
        fit = fits$enrollment_fit,
        newdata = data.frame(arrival_days = rep(0, n_new)),
        t0 = rep(cutoff_offset, n_new),
        fixed_parameters = fixed_parameters
      )
      rand_new_num <- trialsdt_num + enrollment_draw

      new_covariates <- if (length(covars) == 0L) {
        data.frame(row_id = seq_len(n_new))
      } else {
        sample_idx <- sample.int(n_existing, size = n_new, replace = TRUE)
        data[sample_idx, covars, drop = FALSE]
      }

      event_t_new <- simulate_parametric_tte(
        fit = fits$event_fit,
        newdata = new_covariates,
        fixed_parameters = fixed_parameters
      )
      dropout_t_new <- simulate_parametric_tte(
        fit = fits$dropout_fit,
        newdata = new_covariates,
        fixed_parameters = fixed_parameters
      )

      latent_event_new <- rand_new_num + event_t_new - 1
      latent_dropout_new <- rand_new_num + dropout_t_new - 1
      adjusted_event_new <- adjust_event_times(
        rand_time = rand_new_num,
        latent_event_time = latent_event_new,
        assessment_intervals_days = windows$intervals,
        assessment_cut_days = windows$cuts
      )
      observed_new <- adjusted_event_new <= latent_dropout_new
      idx_new <- which(!is.na(observed_new) & observed_new)
      if (length(idx_new) > 0L) {
        sim_events <- c(sim_events, adjusted_event_new[idx_new])
      }
    }

    sim_events <- sort(sim_events)

    if (has_target_events) {
      for (j in seq_along(target_events)) {
        idx <- target_events[j]
        event_date_mat[s, j] <- if (idx <= length(sim_events)) sim_events[idx] else NA_real_
      }
    }

    if (has_target_dates) {
      event_count_mat[s, ] <- vapply(target_date_num, function(x) sum(sim_events <= x), numeric(1))
    }
    plot_mat[s, ] <- vapply(plot_grid_num, function(x) sum(sim_events <= x), numeric(1))

    if (!is.null(pb)) {
      utils::setTxtProgressBar(pb, s)
    }
  }

  date_summary <- if (has_target_events) t(apply(event_date_mat, 2, summarize_ci, ci_level = ci_level)) else NULL
  count_summary <- if (has_target_dates) t(apply(event_count_mat, 2, summarize_ci, ci_level = ci_level)) else NULL
  plot_summary <- t(apply(plot_mat, 2, summarize_ci, ci_level = ci_level))

  pred_dates_tbl <- if (has_target_events) {
    tibble::tibble(
      target_event = target_events,
      pred_date_lcl = as.Date(date_summary[, 1], origin = "1970-01-01"),
      pred_date = as.Date(date_summary[, 2], origin = "1970-01-01"),
      pred_date_ucl = as.Date(date_summary[, 3], origin = "1970-01-01"),
      ci_level = ci_level
    )
  } else {
    NULL
  }

  pred_counts_tbl <- if (has_target_dates) {
    tibble::tibble(
      target_date = target_dates,
      pred_event_lcl = count_summary[, 1],
      pred_event = count_summary[, 2],
      pred_event_ucl = count_summary[, 3],
      ci_level = ci_level
    )
  } else {
    NULL
  }

  plot_df <- tibble::tibble(
    date = plot_grid,
    lcl = plot_summary[, 1],
    med = plot_summary[, 2],
    ucl = plot_summary[, 3]
  )

  actual_df <- tibble::tibble(
    date = plot_grid,
    actual = vapply(plot_grid_num, function(x) sum(observed_events_at_pred_start <= x), numeric(1))
  )
  actual_df <- actual_df[actual_df$date <= pred_start, , drop = FALSE]

  pred_dates <- pred_dates_tbl$pred_date
  x_min <- min(plot_df$date, na.rm = TRUE)
  x_max <- max(plot_df$date, na.rm = TRUE)
  y_max <- max(c(plot_df$ucl, actual_df$actual), na.rm = TRUE)

  pred_date_shapes <- if (has_target_events) {
    lapply(target_events, function(e) {
      list(
        type = "line", xref = "x", yref = "y",
        x0 = x_min, x1 = x_max,
        y0 = e, y1 = e,
        line = list(color = "#ff7f0e", dash = "dot")
      )
    })} else {
      NULL
    }
  pred_date_annotations <- if (has_target_events) {
    lapply(seq_along(target_events), function(i) {
      list(
        x = x_max, y = target_events[i], xref = "x", yref = "y",
        text = sprintf("Target number of events: %s\nPredicted date: %s",
                       as.character(target_events[i]),
                       as.character(pred_dates[i])),
        showarrow = FALSE,
        xanchor = "right",
        yanchor = "top",
        font = list(color = "#ff7f0e", size = 24)
      )
    })} else {
      NULL
    }
  plt <- plotly::plot_ly(plot_df, x = ~date) |>
    plotly::add_ribbons(
      ymin = ~lcl,
      ymax = ~ucl,
      name = paste0(round(ci_level * 100), "% CI"),
      line = list(color = "transparent"),
      fillcolor = "rgba(31,119,180,0.2)"
    ) |>
    plotly::add_lines(y = ~med, name = "Predicted median events", line = list(color = "#1f77b4")) |>
    plotly::layout(
      title = "Predicted Event Trajectory",
      xaxis = list(title = "Date"),
      yaxis = list(title = "Number of Events"),
      shapes = c(list(
        list(
          type = "line", xref = "x", yref = "y",
          x0 = pred_start, x1 = pred_start,
          y0 = 0, y1 = y_max,
          line = list(color = "#d62728", dash = "dash")
        ),
        list(
          type = "line", xref = "x", yref = "y",
          x0 = cutoff_date, x1 = cutoff_date,
          y0 = 0, y1 = y_max,
          line = list(color = "#9467bd", dash = "dash")
        )
      ),
      pred_date_shapes
      ),
      annotations = c(list(
        list(
          x = pred_start, y = y_max, xref = "x", yref = "y",
          text = "Prediction start", showarrow = FALSE,
          xanchor = "left", yanchor = "bottom", font = list(color = "#d62728", size = 24)
        ),
        list(
          x = cutoff_date, y = y_max, xref = "x", yref = "y",
          text = "Cutoff date", showarrow = FALSE,
          xanchor = "left", yanchor = "bottom", font = list(color = "#9467bd", size = 24)
        )
      ),
      pred_date_annotations
      )
    )

  if (nrow(actual_df) > 0L) {
    plt <- plt |>
      plotly::add_lines(
        data = actual_df,
        x = ~date,
        y = ~actual,
        name = "Observed events",
        line = list(color = "#2ca02c"),
        inherit = FALSE
      )
  }

  pred_event_plot <- if (has_target_dates) {
    plotly::plot_ly(pred_counts_tbl, x = ~target_date) |>
      plotly::add_ribbons(
        ymin = ~pred_event_lcl,
        ymax = ~pred_event_ucl,
        name = paste0("Target-date ", round(ci_level * 100), "% CI"),
        line = list(color = "transparent"),
        fillcolor = "rgba(255,127,14,0.25)"
      ) |>
      plotly::add_lines(
        y = ~pred_event,
        name = "Predicted events",
        line = list(color = "#ff7f0e")
      ) |>
      plotly::add_markers(
        y = ~pred_event,
        name = "Predicted events points",
        marker = list(color = "#ff7f0e"),
        showlegend = FALSE
      ) |>
      plotly::layout(
        title = "Predicted Event Counts at Target Dates",
        xaxis = list(title = "Target Date"),
        yaxis = list(title = "Number of Events")
      )
  } else {
    NULL
  }

  out <- list(
    pred_dates = pred_dates_tbl,
    pred_event_counts = pred_counts_tbl,
    plot = plt,
    pred_event_plot = pred_event_plot,
    meta = list(
      prediction_start_date = pred_start,
      cutoff_date = cutoff_date,
      observed_events_at_prediction_start = length(observed_events_at_pred_start),
      observed_events_at_cutoff = length(observed_events_at_cutoff),
      nsim = nsim,
      planned_total_n = planned_total_n
    )
  )
  class(out) <- c("PredRes", class(out))

  if (isTRUE(show_plot)) {
    plot(out)
  }

  out
}
