#' Example Interim ADTTE Data
#'
#' A synthetic ADTTE-style example dataset for package
#' demos/tests. Subject IDs are recoded and dates/durations are perturbed.
#'
#' @format A data frame with 300 rows and 7 columns:
#' \describe{
#'   \item{usubjid}{Unique subject identifier.}
#'   \item{trialsdt}{Trial first-patient randomization date.}
#'   \item{randdt}{Randomization date.}
#'   \item{adt}{Analysis date (event/censor/dropout date).}
#'   \item{cutoffdt}{Data cutoff date.}
#'   \item{status}{Event status: 1 event, 0 censored, -1 dropout.}
#'   \item{aval}{Time from randomization to `adt` in days.}
#' }
#' @source Synthetic example derived from internal development data
"example_adtte"
