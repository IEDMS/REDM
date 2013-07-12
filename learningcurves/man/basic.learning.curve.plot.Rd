\name{basic.learning.curve.plot}
\alias{basic.learning.curve.plot}
\title{Plots a learning curve}
\usage{
  basic.learning.curve.plot(sk, stats, show.fit = F,
    title = lc.title(sk), focus.cutoff = F, show.se = T,
    show.n.students = T)
}
\arguments{
  \item{sk}{the skill object being plotted}

  \item{stats}{a stats object summarizing the learning
  curve. As returned by \code{\link{lc.stats}}}

  \item{show.fit}{whether to display the learning curve fit
  (defaults to \code{FALSE})}

  \item{title}{a title for the plot, defaults to
  \code{\link{lc.title}}}

  \item{focus.cutoff}{whether the plot should hide the
  noisy tail of the curve, as set by
  \code{stats$cutoff.msk}}

  \item{show.n.students}{whether to display the population
  curve as well as the learning curve}
}
\description{
  This function is a wrapper around
  \code{\link{plot.learning.curve}}, setting commonly used
  default options for many parameters.
}

