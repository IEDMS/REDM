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

##############################
##    student simulator
##
## code to simulate student behavior
## and produce simulated skill data

#' pad out the opportunity table with simulated responses
#' 
#' @param opps
#' @param sk.params
#' @param refit.params
#' @return a simulation results object, such as that returned by \code{\link{bkt.sim}}
#' @export
sim.pad.opps <- function( opps, sk.params, refit.params=TRUE )
{
  if( refit.params )
    sk.params = hmm.fit.params( sk.params, opps )
  n.stu = nrow(opps)
  n.opps = ncol(opps)
  pk = pknown( opps, sk.params )
  n.opps.per.stu = apply( !is.na(opps), 1, sum )
  # pad out each student
  padded.opps = opps
  for( i in 1:n.stu ){
    n.opps.to.sim = n.opps - n.opps.per.stu[i]
    if( n.opps.to.sim < 1 )
      next;
    stu.params = sk.params
    stu.params$init = pk[,2:n.opps][i,n.opps.per.stu[i]]
    stu.sim = bkt.sim( 1, n.opps.to.sim, stu.params, mastery.threshold=NULL )
    padded.opps[i,seq(n.opps.per.stu[i]+1,n.opps)] = stu.sim$opps
  }
  padded.opps
}

#' mixed population simulation
#' 
#' @param mastery.params
#' @param k.opps
#' @param n.students
#' @param stu.params
#' @param mastery.threshold
#' @return a simulation result like that returned by \code{\link{bkt.sim}}
#' @export
bkt.mixed.sim <- function( mastery.params, k.opps, n.students, stu.params, mastery.threshold=0.95 )
{
  results = list( opps=c(), known=c(), pknown=c(), mastered=c() )
  for( i in 1:length(n.students) ){
    r = bkt.sim( n.students[i], k.opps, stu.params[i,], mastery.params, mastery.threshold )
    for( n in names(r) )
      results[[n]] = rbind( results[[n]], r[[n]] )
  }
  results
}

#' simulate a population with a single BKT model
#' 
#' @param n.students
#' @param k.opps
#' @param stu.params
#' @param mastery.params
#' @param mastery.threshold
#' @return a list containing the following elements:
#' \itemize{
#'   \item opps
#'   \item known
#'   \item pknown
#'   \item mastered
#' }
#' @export
bkt.sim <- function( n.students, k.opps, stu.params, mastery.params=stu.params, mastery.threshold=0.95 )
{
  # student knowledge at each opportunity
  known = sim.known( n.students, k.opps, stu.params )  
  # resulting student performance table
  opps = matrix( NA, n.students, k.opps )
  # assessment of the mastery system 
  pk = rep( mastery.params$init, n.students )
  
  # performance probabilities
  pc.known = 1.0 - stu.params$slip
  pc.unknown = stu.params$guess
  
  # simulating opportunities
  for( i in 1:k.opps ){
    if( is.numeric(mastery.threshold) )
      mastered = round(pk,2) >= mastery.threshold
    else
      mastered = logical(n.students)
    opps[ !mastered & known[,i], i ] = as.numeric( runif( sum(!mastered & known[,i]) ) < pc.known )
    opps[ !mastered & !known[,i], i ] = as.numeric( runif( sum(!mastered & !known[,i]) ) < pc.unknown )
    pk[!mastered] = pknown.step( opps[!mastered,i], mastery.params, pk[!mastered] )
  }
  results = list( opps=opps, known=known )
  if( is.numeric(mastery.threshold) ){
    results$pknown = pknown( results$opps, mastery.params )
    results$mastered = round(results$pknown,2) >= mastery.threshold
    results$mastered[is.na(results$mastered)] = TRUE
  }
  results
}

#' simulate the known state for a population of students with the given parameters
#' 
#' simulate the sequence in which students transition from unknown to known
#' returns a n.students x k.opps+1 logical matrix
#' where the first column gives the known state prior to any observations
#' subsequent columns give the known state after each opportunity
#' @param n.students
#' @param k.opps
#' @param stu.params
#' @export
#' @keywords internal
sim.known <- function( n.students, k.opps, stu.params )
{
  known = matrix( FALSE, n.students, k.opps+1 )
  known[,1] = runif( n.students ) < stu.params$init
  for( i in 2:(k.opps+1) ){
    already.known = known[,i-1]
    known[ already.known, i ] = TRUE
    known[ !already.known, i ] = runif( sum(!already.known) ) < stu.params$learn
  }
  known
}
