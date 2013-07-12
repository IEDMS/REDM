\name{plot.learning.curve}
\alias{plot.learning.curve}
\title{Plot a learning curve}
\usage{
  plot.learning.curve(p.correct, total, se, cutoff,
    fit = NULL, title = "Learning Curve", focus.cutoff = F,
    draw.cutoff = T, show.n.students = T,
    asymp.upper.bound = NULL, asymp.lower.bound = NULL)
}
\arguments{
  \item{p.correct}{data of the curve to plot (percentage
  correct per opportunity)}

  \item{total}{the total number of responses per
  opportunity}

  \item{se}{the standard error to display around the
  p.correct curve}

  \item{cutoff}{a logical vector masking out opportunities
  not to be displayed}

  \item{fit}{an lm fit object specifying the fit to the
  curve}

  \item{title}{the title to be displayed on the plot}

  \item{focus.cutoff}{whether the plot should hide the
  noisy tail of the curve, as set by
  \code{stats$cutoff.msk}}

  \item{draw.cutoff}{whether the cutoff line should be
  displayed}

  \item{show.n.students}{whether to display the population
  curve as well as the learning curve}

  \item{asymp.upper.bound}{display a dashed line for the
  expected asymptotic upper bound of the learning curve. If
  \code{NULL}, then no line is displayed.}

  \item{asymp.lower.bound}{display a dashed line for the
  expected asymptotic lower bound of the learning curve If
  \code{NULL}, then no line is displayed.}
}
\description{
  Plot a learning curve
}

