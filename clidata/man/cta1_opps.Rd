\name{cta1_opps}
\alias{cta1_opps}
\docType{data}
\title{Sample skill (KC) opportunity tables.}
\description{A list of skill opportunity tables, indexed by section and skill ID.}
\usage{data(cta1_opps)}
\format{
  The format is:
 chr "cta1_opps"
}
\details{Each table is a matrix where each row is the sequence of responses for a given student, 
and the columns are the opportunities on the KC. The values are 1,0 or NA, indicating a correct,
incorrect or no response, respectively.}
\source{
Carnegie Learning Inc. Algebra I Cognitive Tutor, 2009-2010 school year.
}
\examples{
data(toc)
data(cta1_opps)
sk = get.skill("CTA1_01-2","Write expression, mx-1",toc)
sk.opps = get.skill.opps("CTA1_01-2","Write expression, mx-1",cta1.opps)
dim(sk.opps)
sk.opps[1:4,]
}
\keyword{datasets}
