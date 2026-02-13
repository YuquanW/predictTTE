#' Example Interim ADTTE Data (PFS)
#'
#' An anonymized and date-jittered ADTTE-style example dataset for package
#' demos/tests. Subject IDs are recoded and dates/durations are perturbed.
#'
#' @format A data frame with 339 rows and 8 columns:
#' \describe{
#'   \item{usubjid}{Unique subject identifier.}
#'   \item{trialsdt}{Trial first-patient randomization date.}
#'   \item{randdt}{Randomization date.}
#'   \item{adt}{Analysis date (event/censor/dropout date).}
#'   \item{cutoffdt}{Data cutoff date.}
#'   \item{status}{Event status: 1 event, 0 censored, -1 dropout.}
#'   \item{aval}{Time from randomization to `adt` in days.}
#'   \item{endpoint}{Endpoint label.}
#' }
#' @source Synthetic/anonymized example derived from internal development data
"example_adtte_pfs"
