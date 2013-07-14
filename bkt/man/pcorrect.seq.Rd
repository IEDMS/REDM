\name{pcorrect.seq}
\alias{pcorrect.seq}
\title{probability of correct responses predicted by BKT}
\usage{
  pcorrect.seq(opps, params)
}
\arguments{
  \item{opps}{a vector of observed student actions as
  occurs in a single row of an opportunity table}

  \item{params}{a BKT parameter object}
}
\value{
  a vector with the same length as \code{opps}, giving
  running estimates of the probability of a correct
  response predicted by the BKT model with parameters
  \code{params} for the student given the prior actions in
  \code{opps}.
}
\description{
  Calculates the probability of correct responses predicted
  by BKT given the observed actions in a sequence of
  student opportunities
}
\keyword{internal}

