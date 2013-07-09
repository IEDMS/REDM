\name{get.lc.data.rollup}
\alias{get.lc.data.rollup}
\title{Get skill data for a learning curve from a the first_attempt_rollup table}
\usage{
  get.lc.data.rollup(skill, conn = default.connection)
}
\arguments{
  \item{a}{skill object, as a row returned from
  get.curriculum.toc}
}
\value{
  a data.frame of the first-attempts by students on
  subgoals associated with the skill
}
\description{
  This function fetches data suitable for plotting the
  learning curve of a single skill. This query relies on
  the rollup table.
}
\keyword{internal}

