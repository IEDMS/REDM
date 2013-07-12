\name{toc}
\alias{toc}
\docType{data}
\title{Sample TOC object}
\description{A table of contents object. A data.frame with columns describing the skills
and their location in the unit, section hierarchy.}
\usage{data(toc)}
\format{
  The format is:
 chr "toc"
}
\source{
Carnegie Learning Inc. Algebra I Cognitive Tutor, 2009-2010 school year.
}
\examples{
data(toc)
unit.ids(toc)
section.ids.for.unit("CTA1_01",toc)
skills.for.section("CTA1_01","CTA1_01-2",toc)
sk = get.skill("CTA1_01-2","Write expression, mx-1",toc)
}
\keyword{datasets}
