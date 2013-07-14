\name{posterior.pknown}
\alias{posterior.pknown}
\title{updates a BKT estimate of student knowledge}
\usage{
  posterior.pknown(result, params,
    prior.pknown = params$init)
}
\arguments{
  \item{result}{an observed student action}

  \item{params}{a BKT parameter object}

  \item{prior.pknown}{a prior on student knowledge,
  defaults to the init parameter}
}
\value{
  a posterior probability on student knowledge, given the
  parameters and observed data
}
\description{
  applies the BKT algorithm to update the estimate of
  student knowledge given a single observed action
}
\keyword{internal}

