\name{results2opps}
\alias{results2opps}
\title{Transforms results into an opportunity table}
\usage{
  results2opps(res, as.df = T)
}
\arguments{
  \item{res}{table of first-opportunity results}

  \item{as.df}{returns the results as a data.frame if true
  (the default), otherwise it returns a matrix}
}
\value{
  a data.frame (if as.df=T) or matrix where each row
  represents a student, and each column an opportunity to
  practice the skill. Values are 0, 1 or NA indicating
  whether the student produced an (in)correct response at
  the given opportunity, or did not have such an
  opportunity.
}
\description{
  This function transforms a table of first-opportunity
  results, as would be returned from one of the
  get.lc.data.* functions, into an opportunity-table,
  useful for plotting learning curves and analyzing
  learning on the skill.
}
\keyword{internal}

