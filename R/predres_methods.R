#' Show Default Prediction Summary
#'
#' Prints formatted prediction summaries for a `PredRes` object.
#'
#' @param x A `PredRes` object from `predict_tte()`.
#' @param ... Unused.
#' @export
showDefault <- function(x, ...) {
  UseMethod("showDefault")
}

#' @export
showDefault.PredRes <- function(x, ...) {
  if (!inherits(x, "PredRes")) {
    stop("x must be a PredRes object.", call. = FALSE)
  }

  if (!is.null(x$pred_dates)) {
    cat("Predicted dates for targeted event numbers (95% CI)\n")
    out_dates <- data.frame(
      target_event = x$pred_dates$target_event,
      pred_date = x$pred_dates$pred_date,
      ci_lower = x$pred_dates$pred_date_lcl,
      ci_upper = x$pred_dates$pred_date_ucl
    )
    print(out_dates, row.names = FALSE)
  }

  if (!is.null(x$pred_event_counts)) {
    if (!is.null(x$pred_dates)) {
      cat("\n")
    }
    cat("Predicted event counts for targeted dates (95% CI)\n")
    out_counts <- data.frame(
      target_date = x$pred_event_counts$target_date,
      pred_event = x$pred_event_counts$pred_event,
      ci_lower = x$pred_event_counts$pred_event_lcl,
      ci_upper = x$pred_event_counts$pred_event_ucl
    )
    print(out_counts, row.names = FALSE)
  }

  invisible(x)
}

#' @export
print.PredRes <- function(x, ...) {
  showDefault.PredRes(x, ...)
  invisible(x)
}

#' @export
plot.PredRes <- function(x, ...) {
  if (!inherits(x, "PredRes")) {
    stop("x must be a PredRes object.", call. = FALSE)
  }
  if (is.null(x$plot)) {
    stop("No plot is available in this PredRes object.", call. = FALSE)
  }
  print(x$plot)
  invisible(x)
}
