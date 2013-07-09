###
# Copyright (C) 2013
# Carnegie Learning Inc.
#
# This work is made available under the terms of the Creative Commons Attribution-ShareAlike 3.0 license, 
# http://creativecommons.org/licenses/by-sa/3.0/.
#
# This legend must continue to appear in the source code despite
# modifications or enhancements by any party.

### Database Connection Stuff ###

# define module variables
if (!exists("default.connection"))
{
  default.connection <<- NULL
}

#' Connect to a database
#' 
#' This function establishes a connection to a database, and returns the connection object.
#' The connection is also set as the global variable default.connection
#' 
#' @param dbname the name of the database to connect to
#' @param host the hostname of the database server
#' @param usrpass the username and password to login with
#' @param set.default.connection if true, the global variable default.connection will be set
#' @export
connect.to.db <- function( dbname, host='pslc-db', usrpass="dbreader", set.default.connection=TRUE )
{
  drv <- dbDriver("MySQL")
  conn <- dbConnect( drv, host=host,  user=usrpass, password=usrpass, dbname=dbname )
  attr( conn,'has.rollup.table') <- check.rollup.table( dbname, conn )
  if( set.default.connection )
    default.connection <<- conn
  conn
}

check.rollup.table <- function( dbname, conn=default.connection )
{
  query = paste("SELECT * 
                 FROM information_schema.tables 
                 WHERE table_schema = '", dbname ,"' 
                 AND table_name = 'first_attempt_rollup'", sep='' )
  result = dbGetQuery( conn, query )
  ( ! is.null(result) & nrow(result) > 0 )
}

#' Disconnect from a database
#' 
#' This function will close an active database connection.
#' 
#' @param con The connection object to close. Defaults to default.connection
#' @export
disconnect.from.db <- function( conn=default.connection )
{
  if( !is.null(conn) )
  {
    dbDisconnect(conn)
  }
  default.connection <<- NULL
}

#' Retrieves a Table of Contents (TOC) for a particular curriculum
#' 
#' In the context of a Cognitive Tutor curriculum, the Table of Contents (TOC)
#' is a table listing out every unit, section and skill in that curriculum.
#' 
#' @param curriculum a string specifying the curriculum for which we want to fetch a TOC.
#' If null, then a TOC for all curricula will be returned
#' @param con the database connection object to use, will default to default.connection
#' @return a data.frame listing all units, sections and skills, 
#' ordered as they occur in the curriculum
#' @export
get.curriculum.toc <- function( curriculum, conn=default.connection )
{
  query = paste(
   "select ",
    if(!is.null(curriculum)){ "dlsm.sequence as 'unit_number'," },
   "unit.level_name as 'unit_id',
    unit.description as 'unit_name',
    section.level_name as 'section_id',
    section.description as 'section_name',
    skill_name as 'skill_id',
    category as 'skill_cat',
    skill_model_name
    from
    dataset_level unit
    inner join dataset_level section on (unit.dataset_level_id = section.parent_id)
    inner join problem pr on (section.dataset_level_id = pr.dataset_level_id)
    inner join subgoal using (problem_id)
    inner join subgoal_skill_map using (subgoal_id)
    inner join skill using (skill_id) 
    inner join skill_model using (skill_model_id) ",
    if(!is.null(curriculum)){
	    "inner join dataset_level_sequence_map dlsm on (unit.dataset_level_id = dlsm.dataset_level_id) "
    },
   "where skill_model_name = 'KTracedSkills'
    and category in ('tracked','autocreated') ",
    if(!is.null(curriculum)){
	    paste("and dataset_level_sequence_id = 
	          (select dataset_level_sequence_id from dataset_level_sequence where lower(name) = '", tolower(curriculum), "') ", sep='')
    },
   "group by unit.dataset_level_id, section.dataset_level_id, skill_id
    order by ", if(!is.null(curriculum)){"dlsm.sequence, "} else {"unit.dataset_level_id, "},
   "section.dataset_level_id, skill_id", sep='')

	dbGetQuery( conn, query )
}

#' Get skill data for a learning curve
#' 
#' This function fetches data suitable for plotting the learning curve
#' of a single skill. This query does not use the rollup table.
#' 
#' @param a skill object, as a row returned from get.curriculum.toc
#' @return a data.frame of the first-attempts by students on subgoals associated with the skill
#' @export
#' @keywords internal
get.lc.data.norollup <- function( skill, conn=default.connection )
{
  # check skill properties
  if( is.null( skill$section_id ) )
    stop("Skill must contain a section_id")
  if( is.null( skill$skill_id ) )
    stop("Skill must contain a skill_id")
  skill_cat = skill$skill_cat
  if( is.null(skill_cat) )
    skill_cat = 'tracked'
  skill_model_name = skill$skill_model_name
  if( is.null(skill_model_name) )
    skill_model_name = 'KTracedSkills'
  
  # build the query
  query = paste(
   "select
    transaction_time,
    level_name as 'section_id',
    problem_name as 'problem_id',
    subgoal_name,
    skill_name as 'skill_id',
    student_id,
    end_type,
    outcome,
    (outcome='OK') AS 'is_ok'
    from
    dataset_level
    inner join problem using (dataset_level_id)
    inner join subgoal using (problem_id)
    inner join subgoal_skill_map using (subgoal_id)
    inner join skill using (skill_id)  
    inner join tutor_transaction using (subgoal_id)
    inner join session_student_map using (session_id)
    inner join student_section_outcomes using (student_id,dataset_level_id)
    where
    dataset_level_id = 
    ( select dataset_level_id 
      from dataset_level 
      where lower(level_name) = lower('", skill$section_id,"') )
    and
    skill_id = 
    ( select skill_id 
      from skill 
      inner join skill_model using (skill_model_id)
      where lower(skill_name) = lower('", skill$skill_id,"') 
      and category = '", skill_cat,"' 
      and lower(skill_model_name) = lower('", skill_model_name,"') )
    and
    tutor_flag = 'tutor'
    and attempt_at_subgoal=1
    and level_view = 1
    and end_type in ('graduated','promoted')
    order by
    student_id,
    transaction_time", sep='')
  
  # run it
  dbGetQuery( conn, query )
}

#' Get skill data for a learning curve from a the first_attempt_rollup table
#' 
#' This function fetches data suitable for plotting the learning curve
#' of a single skill. This query relies on the rollup table.
#' 
#' @param a skill object, as a row returned from get.curriculum.toc
#' @return a data.frame of the first-attempts by students on subgoals associated with the skill
#' @export
#' @keywords internal
get.lc.data.rollup <- function( skill, conn=default.connection )
{
  # check skill properties
  if( is.null( skill$section_id ) )
    stop("Skill must contain a section_id")
  if( is.null( skill$skill_id ) )
    stop("Skill must contain a skill_id")
  
  query = paste(
    "SELECT
    student_id,
    end_type,
    skill_name,
    outcome,
    level_name AS 'section_name',
    subgoal_name,
    problem_name,
    attempt_time,
    (outcome='OK') AS 'is_ok'
    FROM
    first_attempt_rollup
    INNER JOIN dataset_level ON (section_id = dataset_level_id)
    INNER JOIN student_section_outcomes USING (student_id, dataset_level_id)
    INNER JOIN problem USING (problem_id)
    INNER JOIN subgoal USING (subgoal_id)
    INNER JOIN skill USING (skill_id)  
    WHERE
    section_id = (SELECT dataset_level_id FROM dataset_level WHERE lower(level_name) = lower('", skill$section_id,"'))
    AND skill_id = (SELECT skill_id FROM skill WHERE lower(skill_name) = lower('", skill$skill_id,"') and category='tracked' and skill_model_id=2)
    AND tutor_flag = 'tutor'
    AND level_view = 1
    AND end_type in ('graduated','promoted')
    ORDER BY
    student_id, attempt_time", sep='')
  
  dbGetQuery( conn, query )
}

#' Transforms results into an opportunity table
#' 
#' This function transforms a table of first-opportunity results,
#' as would be returned from one of the get.lc.data.* functions,
#' into an opportunity-table, useful for plotting learning curves
#' and analyzing learning on the skill.
#' 
#' @param res table of first-opportunity results
#' @param as.df returns the results as a data.frame if true (the default), otherwise it returns a matrix
#' @return a data.frame (if as.df=T) or matrix where each row represents a student, 
#' and each column an opportunity to practice the skill. Values are 0, 1 or NA indicating whether the 
#' student produced an (in)correct response at the given opportunity, or did not have such an opportunity.
#' @export
#' @keywords internal
results2opps <- function( res, as.df=T )
{
  student_ids = unique(res$student_id)
  max_opps = 0
  num_students = length(student_ids)
  for( i in 1:num_students ){
    max_opps = max( max_opps, sum(res$student_id == student_ids[i]))
  }
  opps = matrix(nrow=num_students,ncol=max_opps)
  for( i in 1:num_students ){
    msk = res$student_id == student_ids[i]
    opps[i,1:sum(msk)] = res$is_ok[msk]
  }
  if( as.df )
    return( as.data.frame( opps, row.names=as.integer(student_ids) ) )
  opps
}

#' Get skill data for a learning curve
#' 
#' This function fetches data suitable for plotting the learning curve of a single skill.
#' 
#' @param a skill object, as a row returned from get.curriculum.toc
#' @return a data.frame (if as.df=T) or matrix where each row represents a student, 
#' and each column an opportunity to practice the skill. Values are 0, 1 or NA indicating whether the 
#' student produced an (in)correct response at the given opportunity, or did not have such an opportunity.
#' @export
get.skill.opps <- function( skill, conn=default.connection )
{
  # select out of the rollup table if we're using the default skills
  if( attr(conn,'has.rollup.table') 
      & skill$skill_cat == 'tracked' 
      & skill$skill_model_name == 'KTracedSkills' ){
		return( results2opps( get.lc.data.rollup( skill, conn ) ) )
	}
  results2opps( get.lc.data.norollup( skill, conn ) )
}
