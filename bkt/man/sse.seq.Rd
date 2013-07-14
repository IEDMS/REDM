\name{sse.seq}
\alias{sse.seq}
\title{#' sum of squared errors of the BKT model}
\usage{
  sse.seq(seq, params)
}
\arguments{
  \item{seq}{a vector of observed student actions as occurs
  in a single row of an opportunity table}

  \item{params}{a BKT parameter object}
}
\value{
  the sum of squared errors for the BKT model's predictions
  on this data
}
\description{
  Calculates a sum of squared errors for the BKT model with
  the given parameters on the given data.
}
\keyword{internal}

