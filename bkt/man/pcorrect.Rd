\name{pcorrect}
\alias{pcorrect}
\title{probability of correct responses predicted by BKT}
\usage{
  pcorrect(opps, params)
}
\arguments{
  \item{opps}{the opportunity table of observed student
  actions}

  \item{params}{a BKT parameter object}
}
\value{
  a matrix with the same dimensions as \code{opps}, giving
  the running estimates of the probability of a correct
  response predicted by the BKT model with parameters
  \code{params} for the student represented by the
  corresponding row immediately prior to corresponding
  column in \code{opps}. That is, the if the \code{(i,j)}
  entry in the matrix had a value of 0.7, this means that
  the BKT algorithm with the given parameters predicts that
  the i-th student has a 70 percent chance of producing a
  correct response on the j-th entry, given the j-1
  previously observed actions.
}
\description{
  Calculates the probability of correct responses predicted
  by BKT given the observed actions in a table of student
  opportunities
}

