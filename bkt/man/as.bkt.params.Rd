\name{as.bkt.params}
\alias{as.bkt.params}
\title{coerces various data types to a standard format for BKT parameters}
\usage{
  as.bkt.params(p, round.params = T)
}
\arguments{
  \item{p}{the object to be coerced}

  \item{round.params}{whether we want to round off the
  parameter values to 2 decimal-places.}
}
\value{
  a data.frame with four named columns: \itemize{ \item
  init \item learn \item guess \item slip }
}
\description{
  coerces various data types to a standard format for BKT
  parameters
}

