\name{plot.bkt.space}
\alias{plot.bkt.space}
\title{Plots the BKT space as a heatmap}
\usage{
  plot.bkt.space(bf, x.axis = "init", y.axis = "learn",
    title = paste(x.axis, "X", y.axis, "parameter space"))
}
\arguments{
  \item{bf}{a brute-force search results object, as
  returned by \code{\link{bforce.search}}}

  \item{x.axis}{the parameter to be plotted along the
  horizontal axis, defaults to \code{"init"}.}

  \item{y.axis}{the parameter to be plotted along the
  vertical axis, defaults to \code{"learn"}.}

  \item{title}{the title for the plot}
}
\description{
  Plots the BKT space as a heatmap
}

