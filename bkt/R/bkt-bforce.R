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

#' Generates a search grid for brute-force fitting
#' 
#' @param step.size the incremental change in each parameter
#' @param max.init the maximum init value
#' @param max.learn the maximum learn value
#' @param max.guess the maximum guess value
#' @param max.slip the maximum slip value
#' @return a Nx4 numeric matrix, each row being a set of bkt parameters to try in the brute force search
#' @export
bforce.search.grid <- function( step.size=0.1, 
                                max.init=(1.0 - step.size), 
                                max.learn=(1.0 - step.size), 
                                max.guess=(0.5 - step.size), 
                                max.slip=(0.5 - step.size) )
{
	all.init = seq( step.size, max.init, step.size )
	n.init = length(all.init)
	all.learn = seq( step.size, max.learn, step.size )
	n.learn = length(all.learn)	
	all.guess = seq( step.size, max.guess, step.size )
	n.guess = length(all.guess)	
	all.slip = seq( step.size, max.slip, step.size )
	n.slip = length(all.slip)	
	as.matrix( data.frame( init=rep(all.init, times=(n.learn*n.guess*n.slip) ), 
				learn=rep(all.learn, each=n.init, times=(n.guess*n.slip) ), 
				guess=rep(all.guess, each=(n.init*n.learn), times=n.slip), 
				slip=rep(all.slip, each=(n.init*n.learn*n.guess) ) ) )
}

#' Performs a brute-force search of the parameter space
#' 
#' @param opps the opportunity table 
#' @param search.grid an Nx4 matrix of BKT parameters to try, as returned by \code{\link{bforce.search.grid}} (the default).
#' @param fit.metric the metric function to use. Must have the same profile as \code{\link{rmse}}, the default.
#' That is, it should take an opportunity table and set of parameters as arguments, and return a numeric value.
#' @param minimize whether the optimal value is the minimum (the default), or the maximum.
#' @param parallel whether to parallelize the search. Requires that an appropriate parallelization backend is initialized. See \code{\link{foreach}} for more details.
#' @return a list with the following elements:
#' \itemize{
#'   \item search.grid the values searched
#'   \item fit.metric the fit metric function used
#'   \item fits the result of applying the metric function to each row in the search grid
#'   \item best.fit the optimal (min or max) metric value
#'   \item best.params the parameters corresponding to the optimal metric value
#' }
#' @export
bforce.search <- function( opps, search.grid=bforce.search.grid(), fit.metric=rmse, minimize=T, parallel=FALSE )
{
  bf = list( search.grid=search.grid, fit.metric=fit.metric )
	if( parallel )
		bf$fits = as.double(foreach( i = 1:dim(search.grid)[1] ) %dopar% fit.metric( opps, search.grid[i,] ))
	else
	  bf$fits = apply( search.grid, 1, function(p){ fit.metric( opps, p ) } )
  if( minimize )
    bf$best.fit = min(bf$fits)
  else
    bf$best.fit = max(bf$fits)
  bf$best.params = as.bkt.params(bf$search.grid[ bf$fits == bf$best.fit ])
  bf
}

#' Plots the BKT space as a heatmap
#' 
#' @param bf a brute-force search results object, as returned by \code{\link{bforce.search}}
#' @param x.axis the parameter to be plotted along the horizontal axis, defaults to \code{"init"}.
#' @param y.axis the parameter to be plotted along the vertical axis, defaults to \code{"learn"}.
#' @param title the title for the plot
#' @export
plot.bkt.space <- function( bf, x.axis="init", y.axis="learn", title=paste(x.axis,'X',y.axis,'parameter space') )
{
	axis.vals1 = unique(bf$search.grid[,x.axis])
	axis.vals2 = unique(bf$search.grid[,y.axis])
	n_x = length(axis.vals1)
	n_y = length(axis.vals2)
	avg.fit = matrix(data=0,nrow=n_x,ncol=n_y)
	for( i in 1:n_x ){
		for( j in 1:n_y ){
			param.msk = bf$search.grid[,x.axis] == axis.vals1[i] & bf$search.grid[,y.axis] == axis.vals2[j]
			avg.fit[i,j] = mean( bf$fits[param.msk], na.rm=T )
		}
	}
	image( axis.vals1, axis.vals2, avg.fit, xlab=x.axis, ylab=y.axis, xlim=c(0,1.0), ylim=c(0,1.0) )
  title(title)
}
