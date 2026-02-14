\name{predict_tte}
\alias{predict_tte}
\title{Predict Event Counts and Dates from ADTTE Data}
\usage{
predict_tte(
  data,
  planned_total_n,
  target_events,
  target_dates = NULL,
  nsim = 1000,
  assessment_intervals_days = NULL,
  assessment_cut_days = NULL,
  enrollment_model = "exponential",
  event_model = "exponential",
  dropout_model = "exponential",
  ci_level = 0.95,
  covariates = NULL,
  fixed_parameters = TRUE,
  seed = NULL,
  show_progress = TRUE,
  show_plot = FALSE
)
}
\arguments{
  \item{data}{ADTTE data with required columns: \code{usubjid}, \code{trialsdt}, \code{randdt}, \code{adt}, \code{cutoffdt}, \code{status}, \code{aval}.}
  \item{planned_total_n}{Planned total sample size.}
  \item{target_events}{Target event counts to predict dates for. Can be \code{NULL} if \code{target_dates} is provided.}
  \item{target_dates}{Target dates to predict event counts for. Can be \code{NULL} if \code{target_events} is provided.}
  \item{nsim}{Number of simulation runs.}
  \item{assessment_intervals_days}{Piecewise assessment interval lengths in days; set \code{NULL} to disable adjustment.}
  \item{assessment_cut_days}{Cut points (days since randomization) where interval changes. Can be \code{NULL}.}
  \item{enrollment_model}{Enrollment model distribution.}
  \item{event_model}{Event model distribution.}
  \item{dropout_model}{Dropout model distribution.}
  \item{ci_level}{Confidence interval level.}
  \item{covariates}{Optional covariate names for event/dropout models.}
  \item{fixed_parameters}{If \code{TRUE}, use point estimates; if \code{FALSE}, sample coefficients from asymptotic distribution.}
  \item{seed}{Optional random seed.}
  \item{show_progress}{Logical; if \code{TRUE}, show a simulation progress bar.}
  \item{show_plot}{Logical; if \code{TRUE}, display the prediction plot after simulation.}
}
\value{
A \code{PredRes} object containing:
\itemize{
  \item \code{pred_dates}: predicted dates and confidence intervals for target event counts.
  \item \code{pred_event_counts}: predicted event counts and confidence intervals for target dates.
  \item \code{plot}: interactive event trajectory plot.
  \item \code{pred_event_plot}: interactive CI-ribbon plot for target-date event predictions.
  \item \code{meta}: run metadata.
}
}
\description{
Simulates future enrollment, event, and dropout outcomes from interim ADTTE data,
then summarizes predictions for user-specified milestones. The prediction start
date is defined as the earliest censoring date among ongoing subjects.
}
