\name{example_adtte}
\docType{data}
\alias{example_adtte}
\title{Example Interim ADTTE Data}
\format{
A data frame with 339 rows and 7 columns:
\describe{
  \item{usubjid}{Unique subject identifier.}
  \item{trialsdt}{Trial first-patient randomization date.}
  \item{randdt}{Randomization date.}
  \item{adt}{Analysis date (event/censor/dropout date).}
  \item{cutoffdt}{Data cutoff date.}
  \item{status}{Event status: 1 event, 0 censored, -1 dropout.}
  \item{aval}{Time from randomization to adt in days.}
}
}
\source{
Synthetic/anonymized example derived from internal development data.
}
\description{
An anonymized and date-jittered ADTTE-style example dataset for demos/tests.
}
\keyword{datasets}
