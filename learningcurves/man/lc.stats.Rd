\name{lc.stats}
\alias{lc.stats}
\title{Summary statistics for a learning curve}
\usage{
  lc.stats(opps, cutoff.fn = find.cutoff.msk,
    fit.fn = fit.learning.curve, estimate.se = T)
}
\arguments{
  \item{opps}{the opportunity table to summarize}

  \item{cutoff.fn}{a heurisitic function to determine
  whether/how to trim the opportunity table. The function
  should obey the profile of \code{\link{find.cutoff.msk}}
  (the default). That is, it should take a numeric vector
  as an argument and return a logical vector of the same
  length.}

  \item{fit.fn}{a function to compute a fit to the curve.
  The function should obey the profile of
  \code{\link{fit.learning.curve}} (the default). That is,
  it should take a stats object as an argument, and return
  an lm or glm fit object.}

  \item{estimate.se}{whether we should compute a bootstrap
  estimation of the standard-error at each opportunity}
}
\value{
  a stats object. Specifically, a list with the following
  elements: \itemize{ \item total a numeric vector giving
  the total number of responses observed per opportunity
  \item correct a numeric vector giving the number of
  correct reponses per opportunity \item p.correct a
  numeric vector giving percentage of correct responses per
  opportunity \item p.error a numeric vector giving
  percentage of incorrect responses per opportunity \item
  cutoff.msk a logical vector which acts as a mask over the
  above vectors to trim off the noisy tail of the curve. As
  returned by the provided \code{\link{cutoff.fn}}.  \item
  fit an lm fit object giving the fit to the learning
  curve, as returned by the provided \code{\link{fit.fn}}
  \item se if \code{estimate.se}, then this element will be
  a numeric vector giving the bootstrap estimates of
  standard error in the p.correct (or p.error) values.
  Values are \code{NA} when there is insufficient
  observations to support a reasonable bootstrap. }
}
\description{
  Summary statistics for a learning curve
}

