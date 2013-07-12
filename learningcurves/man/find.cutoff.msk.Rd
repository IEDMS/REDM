\name{find.cutoff.msk}
\alias{find.cutoff.msk}
\title{Heuristic for cutting off the noisy tail of a learning curve}
\usage{
  find.cutoff.msk(total, init.cutoff = 0.1,
    drop.cutoff = NULL)
}
\arguments{
  \item{total}{a vector representing the number of
  observations at each observation}

  \item{init.cutoff}{the initial population heuristic
  parameter. If \code{NULL}, then this heuristic will not
  be used, otherwise a number in \code{[0,1]}. Defaults to
  \code{0.1}}

  \item{drop.cutoff}{the drop heuristic parameter. If
  \code{NULL} (the default), then this heuristic will not
  be used, otherwise a number in \code{[0,1]}.}
}
\value{
  a logical vector which acts as a mask over the
  opportunities, \code{TRUE} for opportunites to be
  included, \code{FALSE} for those after the cutoff point.
}
\description{
  We can trim the noisy tail off the curve for the purposes
  of plotting, and curve fitting. Two heuristic criteria
  are used to determine the cutoff opportunity. The initial
  population heuristic will trim the curve when the sample
  population falls below some fraction of the initial
  population. The drop heuristic will trim the curve if the
  drop in sample population from any opportunity to the
  next is more than some fraction. These heuristics are
  parameterized by \code{init.cutoff} and
  \code{drop.cutoff}, respectively. If both heurisitics are
  used, then the earliest cutoff point is used.
}

