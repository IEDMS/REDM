##########################################################################################################
# Copyright (C) 2013
# Carnegie Learning Inc.
#
# This work is made available under the terms of the Creative Commons Attribution-ShareAlike 3.0 license, 
# http://creativecommons.org/licenses/by-sa/3.0/.
#
# This legend must continue to appear in the source code despite
# modifications or enhancements by any party.
##########################################################################################################

### TOC functions ###

#' Fetches a unit from a TOC
#' 
#' This function fetches the subset of a TOC object for a particular unit
#' 
#' @param unit.id the ID of the unit to fetch
#' @param toc the TOC object to query
#' @return a unit TOC: a data.frame representing the subset of the 
#' TOC object representing just the skills in the specified unit
#' @export
get.unit <- function( unit.id, toc )
{
  toc[ toc$unit_id == unit.id, ]
}

#' Fetches a section from a TOC
#' 
#' This function fetches the subset of a TOC object for a particular section
#' 
#' @param section.id the ID of the section to fetch
#' @param toc the TOC object to query
#' @return a data.frame representing the subset of the TOC object representing just the skills
#' in the specified section
#' @export
get.section <- function( section.id, toc )
{
  toc[ toc$section_id == section.id, ]
}

#' Fetches a skill from a TOC
#' 
#' This function fetches the subset of a TOC object for a particular skill
#' 
#' @param section.id the ID of the section in which the skill occurs
#' @param skill.id the ID of the skill to fetch
#' @param toc the TOC object to query
#' @return a data.frame representing the subset of the TOC object representing just the specified skill
#' @export
get.skill <- function( section.id, skill.id, toc )
{
  toc[ (toc$section_id == section.id & toc$skill_id == skill.id), ]
}

#' Lists the units in a TOC
#' 
#' This function lists the unique unit IDs in a TOC
#' 
#' @param toc the TOC object to query
#' @return a list of the unique unit IDs in the TOC
#' @export
unit.ids <- function( toc )
{
  unique( toc$unit_id )
}

#' Lists the unit names in a TOC
#' 
#' This function lists the unique unit names in a TOC
#' 
#' @param toc the TOC object to query
#' @return a list of the unique unit names in the TOC
#' @export
unit.names <- function( toc )
{
  unique( toc$unit_name )
}

#' Lists the sections in a unit
#' 
#' This function lists the unique section IDs in a TOC
#' 
#' @param unit.id the unit ID from which the sections are to be draw
#' @param toc the TOC object to query
#' @return a list of the unique section IDs in the TOC within the specified unit
#' @export
section.ids.for.unit <- function( unit.id, toc )
{
  unique( get.unit( unit.id, toc )[,'section_id'] )
}

#' Lists the section names in a unit
#' 
#' This function lists the unique section names in a TOC
#' 
#' @param unit.id the unit ID from which the sections are to be draw
#' @param toc the TOC object to query
#' @return a list of the unique section names in the TOC within the specified unit
#' @export
section.names.for.unit <- function( unit.id, toc )
{
  unique( get.unit( unit.id, toc )[,'section_name'] )
}

#' Lists the skills in a section
#' 
#' This function lists the unique unit.ids in a TOC
#' 
#' @param toc the TOC object to query
#' @return a list of the unique unit IDs in the TOC
#' @export
skills.for.section <- function( unit.id, section.id, toc )
{
  get.section( section.id, get.unit( unit.id, toc ) )
}


#' Fetches a skill from a TOC
#' 
#' This function fetches the subset of a TOC object for a particular skill
#' 
#' @param section.id the ID of the section in which the skill occurs
#' @param skill.id the ID of the skill to fetch
#' @param toc the TOC object to query
#' @return a data.frame representing the subset of the TOC object representing just the specified skill
#' @export
get.skill.opps <- function( section.id, skill.id, data )
{
  data[[ paste( section.id, skill.id, sep='.' ) ]]
}
