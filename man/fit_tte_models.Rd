\name{fit_tte_models}
\alias{fit_tte_models}
\title{Fit Enrollment, Event, and Dropout Models}
\usage{
fit_tte_models(
  data,
  enrollment_model = "exponential",
  event_model = "exponential",
  dropout_model = "exponential",
  covariates = NULL
)
}
\arguments{
  \item{data}{ADTTE data frame with required columns.}
  \item{enrollment_model}{Parametric distribution for enrollment model.}
  \item{event_model}{Parametric distribution for event model.}
  \item{dropout_model}{Parametric distribution for dropout model.}
  \item{covariates}{Optional covariate column names for event/dropout models.}
}
\value{
A list containing fitted \code{survreg} model objects.
}
\description{
Fits parametric enrollment, event, and dropout models used by \code{predict_tte()}.
Supported distributions include exponential, weibull, log-normal, and log-logistic.
}
