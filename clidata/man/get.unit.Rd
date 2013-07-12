\name{get.unit}
\alias{get.unit}
\title{Fetches a unit from a TOC}
\usage{
  get.unit(unit.id, toc)
}
\arguments{
  \item{unit.id}{the ID of the unit to fetch}

  \item{toc}{the TOC object to query}
}
\value{
  a unit TOC: a data.frame representing the subset of the
  TOC object representing just the skills in the specified
  unit
}
\description{
  This function fetches the subset of a TOC object for a
  particular unit
}

