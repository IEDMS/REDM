\name{get.skill}
\alias{get.skill}
\title{Fetches a skill from a TOC}
\usage{
  get.skill(section.id, skill.id, toc)
}
\arguments{
  \item{section.id}{the ID of the section in which the
  skill occurs}

  \item{skill.id}{the ID of the skill to fetch}

  \item{toc}{the TOC object to query}
}
\value{
  a data.frame representing the subset of the TOC object
  representing just the specified skill
}
\description{
  This function fetches the subset of a TOC object for a
  particular skill
}

