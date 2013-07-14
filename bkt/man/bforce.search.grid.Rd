\name{bforce.search.grid}
\alias{bforce.search.grid}
\title{Generates a search grid for brute-force fitting}
\usage{
  bforce.search.grid(step.size = 0.1,
    max.init = (1 - step.size),
    max.learn = (1 - step.size),
    max.guess = (0.5 - step.size),
    max.slip = (0.5 - step.size))
}
\arguments{
  \item{step.size}{the incremental change in each
  parameter}

  \item{max.init}{the maximum init value}

  \item{max.learn}{the maximum learn value}

  \item{max.guess}{the maximum guess value}

  \item{max.slip}{the maximum slip value}
}
\value{
  a Nx4 numeric matrix, each row being a set of bkt
  parameters to try in the brute force search
}
\description{
  Generates a search grid for brute-force fitting
}

