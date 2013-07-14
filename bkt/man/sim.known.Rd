\name{sim.known}
\alias{sim.known}
\title{simulate the known state for a population of students with the given parameters}
\usage{
  sim.known(n.students, k.opps, stu.params)
}
\arguments{
  \item{n.students}{}

  \item{k.opps}{}

  \item{stu.params}{}
}
\description{
  simulate the sequence in which students transition from
  unknown to known returns a n.students x k.opps+1 logical
  matrix where the first column gives the known state prior
  to any observations subsequent columns give the known
  state after each opportunity
}
\keyword{internal}

