\name{pknown.seq}
\alias{pknown.seq}
\title{probability of student knowledge predicted by BKT}
\usage{
  pknown.seq(opps, params)
}
\arguments{
  \item{opps}{a vector of observed student actions as
  occurs in a single row of an opportunity table}

  \item{params}{a BKT parameter object}
}
\value{
  a vector with the same length as \code{opps}, giving
  running estimates of the probability of student knowledge
  predicted by the BKT model with parameters \code{params}
  for the student given the prior actions in \code{opps}.
}
\description{
  Calculates the probability of a student being in the
  known state, predicted by BKT given the observed actions
  in a sequence of student opportunities
}
\keyword{internal}

