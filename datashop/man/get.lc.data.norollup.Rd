\name{get.lc.data.norollup}
\alias{get.lc.data.norollup}
\title{Get skill data for a learning curve}
\usage{
  get.lc.data.norollup(skill, conn = default.connection)
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
  learning curve of a single skill. This query does not use
  the rollup table.
}
\keyword{internal}

