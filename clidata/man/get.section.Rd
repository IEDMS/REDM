\name{get.section}
\alias{get.section}
\title{Fetches a section from a TOC}
\usage{
  get.section(section.id, toc)
}
\arguments{
  \item{section.id}{the ID of the section to fetch}

  \item{toc}{the TOC object to query}
}
\value{
  a data.frame representing the subset of the TOC object
  representing just the skills in the specified section
}
\description{
  This function fetches the subset of a TOC object for a
  particular section
}

