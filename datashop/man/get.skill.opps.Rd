\name{get.skill.opps}
\alias{get.skill.opps}
\title{Get skill data for a learning curve}
\usage{
  get.skill.opps(skill, conn = default.connection)
}
\arguments{
  \item{a}{skill object, as a row returned from
  get.curriculum.toc}
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
  This function fetches data suitable for plotting the
  learning curve of a single skill.
}

