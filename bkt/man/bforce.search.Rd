\name{bforce.search}
\alias{bforce.search}
\title{Performs a brute-force search of the parameter space}
\usage{
  bforce.search(opps, search.grid = bforce.search.grid(),
    fit.metric = rmse, minimize = T, parallel = FALSE)
}
\arguments{
  \item{opps}{the opportunity table}

  \item{search.grid}{an Nx4 matrix of BKT parameters to
  try, as returned by \code{\link{bforce.search.grid}} (the
  default).}

  \item{fit.metric}{the metric function to use. Must have
  the same profile as \code{\link{rmse}}, the default. That
  is, it should take an opportunity table and set of
  parameters as arguments, and return a numeric value.}

  \item{minimize}{whether the optimal value is the minimum
  (the default), or the maximum.}

  \item{parallel}{whether to parallelize the search.
  Requires that an appropriate parallelization backend is
  initialized. See \code{\link{foreach}} for more details.}
}
\value{
  a list with the following elements: \itemize{ \item
  search.grid the values searched \item fit.metric the fit
  metric function used \item fits the result of applying
  the metric function to each row in the search grid \item
  best.fit the optimal (min or max) metric value \item
  best.params the parameters corresponding to the optimal
  metric value }
}
\description{
  Performs a brute-force search of the parameter space
}

