\name{get.curriculum.toc}
\alias{get.curriculum.toc}
\title{Retrieves a Table of Contents (TOC) for a particular curriculum}
\usage{
  get.curriculum.toc(curriculum, conn = default.connection)
}
\arguments{
  \item{curriculum}{a string specifying the curriculum for
  which we want to fetch a TOC. If null, then a TOC for all
  curricula will be returned}

  \item{con}{the database connection object to use, will
  default to default.connection}
}
\value{
  a data.frame listing all units, sections and skills,
  ordered as they occur in the curriculum
}
\description{
  In the context of a Cognitive Tutor curriculum, the Table
  of Contents (TOC) is a table listing out every unit,
  section and skill in that curriculum.
}

