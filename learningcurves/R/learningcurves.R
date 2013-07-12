### Functions for plotting & working with learning curves ###

#' The number of correct responses at each opportunity
#' 
#' Calculates the number of correct responses at each opportunity 
#' across all the students in an opportunity table
#' 
#' @param opps an opportunity table
#' @return a numeric vector with length equal to the number of rows in opps, 
#' giving the number of correct responses at each opportunity
#' @export
get.correct.from.opps <- function( opps )
{
  if( is.vector(opps) ){
    correct = sum(opps, na.rm=T )
  } else {
    correct = apply(opps,2,sum,na.rm=T)
  }
  return(correct)
}

#' The total number of responses at each opportunity
#' 
#' Calculates the total number of responses at each opportunity 
#' across all the students in an opportunity table
#' 
#' @param opps an opportunity table
#' @return a numeric vector with length equal to the number of rows in opps, 
#' giving the total number of responses at each opportunity
#' @export
get.total.from.opps <- function( opps )
{
  if( is.vector(opps) ){
    total = sum(!is.na(opps))
  } else {
    total = apply(!is.na(opps),2,sum)
  }
  return(total)
}

#' Heuristic for cutting off the noisy tail of a learning curve
#' 
#' We can trim the noisy tail off the curve for the purposes of plotting, and curve fitting.
#' Two heuristic criteria are used to determine the cutoff opportunity. The initial population
#' heuristic will trim the curve when the sample population falls below some fraction of the 
#' initial population. The drop heuristic will trim the curve if the drop in sample population
#' from any opportunity to the next is more than some fraction. These heuristics are parameterized
#' by \code{init.cutoff} and \code{drop.cutoff}, respectively. If both heurisitics are used, then
#' the earliest cutoff point is used.
#' 
#' @param total a vector representing the number of observations at each observation
#' @param init.cutoff the initial population heuristic parameter. If \code{NULL}, then this heuristic
#' will not be used, otherwise a number in \code{[0,1]}. Defaults to \code{0.1}
#' @param drop.cutoff the drop heuristic parameter. If \code{NULL} (the default), then this 
#' heuristic will not be used, otherwise a number in \code{[0,1]}.
#' @return a logical vector which acts as a mask over the opportunities, \code{TRUE} for opportunites
#' to be included, \code{FALSE} for those after the cutoff point.
#' @export
find.cutoff.msk <- function( total, init.cutoff=0.1, drop.cutoff=NULL )
{
  n = length(total)
  cutoff.msk = rep(TRUE,n)
  if( is.numeric(init.cutoff) )
    cutoff.msk = total > init.cutoff*total[1]
  if( is.numeric(drop.cutoff) ){
    cur = 1:(n-1)
    nxt = 2:n
    cutoff.msk[nxt] = cutoff.msk[nxt] & (total[cur]-total[nxt]) < drop.cutoff*total[cur]
  }
  if( any(!cutoff.msk) )
    cutoff.msk[min(seq(1,n)[!cutoff.msk]):n] = FALSE
  cutoff.msk
}

## fitting a power-law function to the curve ##

lc.fitting.data <- function( stats )
{
  has.opps = stats$total > 0
  opp.nums = c(1:sum(has.opps))
  lc.data <- data.frame( log_opp = log(opp.nums), log_error = as.double(log(stats$p.error[has.opps])) )
  lc.data$log_error[ is.infinite( lc.data$log_error ) ] = NA
  lc.data
}

#' Performs a power-law fit to the learning curve
#' 
#' @param stats a learning curve summary statistics object, as returned by \code{lc.stats}
#' @return a lm object for the power-law fit
#' @export
fit.learning.curve <- function( stats )
{
  lc.data <- lc.fitting.data( stats )
  if( sum(!is.na(lc.data$log_error)) < 1 ){
    return( NULL )
  }
  lm( log_error ~ log_opp, data = lc.data, weights = stats$total )
}

#' Format a learning curve plot title
#' 
#' Utility function to produce a title suitable for a learning curve of the given skill.
#' @param sk the skill for which to generate a title
#' @return a string with a nicely formatted title
lc.title <- function( sk )
{
  paste("Learning Curve\nUnit: ", sk$unit_desc, ", Section: ", sk$section_desc, "\nSkill: ", sk$skill_name )
}

#' Summary statistics for a learning curve
#' 
#' @param opps the opportunity table to summarize
#' @param cutoff.fn a heurisitic function to determine whether/how to trim
#' the opportunity table. The function should obey the profile of \code{\link{find.cutoff.msk}} (the default).
#' That is, it should take a numeric vector as an argument and return a logical vector of the same length.
#' @param fit.fn a function to compute a fit to the curve. The function should obey the profile
#' of \code{\link{fit.learning.curve}} (the default). That is, it should take a stats object as an argument,
#' and return an lm or glm fit object.
#' @param estimate.se whether we should compute a bootstrap estimation of the standard-error at each opportunity
#' @return a stats object. Specifically, a list with the following elements:
#' \itemize{
#'   \item total a numeric vector giving the total number of responses observed per opportunity
#'   \item correct a numeric vector giving the number of correct reponses per opportunity
#'   \item p.correct a numeric vector giving percentage of correct responses per opportunity
#'   \item p.error a numeric vector giving percentage of incorrect responses per opportunity
#'   \item cutoff.msk a logical vector which acts as a mask over the above vectors to trim off the 
#'   noisy tail of the curve. As returned by the provided \code{\link{cutoff.fn}}.
#'   \item fit an lm fit object giving the fit to the learning curve, as returned by the provided \code{\link{fit.fn}}
#'   \item se if \code{estimate.se}, then this element will be a numeric vector giving the bootstrap estimates
#'   of standard error in the p.correct (or p.error) values. Values are \code{NA} when there is insufficient observations
#'   to support a reasonable bootstrap.
#' }
#' @export
lc.stats <- function( opps, cutoff.fn=find.cutoff.msk, fit.fn=fit.learning.curve, estimate.se=T )
{
  lc <- list( total=get.total.from.opps( opps ),
              correct=get.correct.from.opps( opps ) )
  lc$p.correct = 100*lc$correct/lc$total
  lc$p.error = 100 - lc$p.correct
  if( ! is.null(cutoff.fn) ){
    lc$cutoff.msk = cutoff.fn( lc$total )
  } else {
    lc$cutoff.msk = rep(TRUE,length(lc$total))
  }
  if( ! is.null(fit.fn) )
    lc$fit <- fit.fn( lc )
  if( estimate.se ){
    lc$se = double( length(lc$total) )
    lc$se[] = NA
    num.opps.to.sample = sum( lc$total[lc$cutoff.msk] >= 10 )
    if( num.opps.to.sample < 1 ){ return(lc) }
    for( i in 1:num.opps.to.sample ){
      pc.boot = boot( opps[,i], function(d,i){ 100*get.correct.from.opps(d[i])/get.total.from.opps(d[i]) }, 999 )
      lc$se[i] = sd(as.vector(pc.boot$t))
    }
  }
  lc
}

## plot a learning curve ##

#' Plots a learning curve
#' 
#' This function is a wrapper around \code{\link{plot.learning.curve}}, setting 
#' commonly used default options for many parameters.
#' @param sk the skill object being plotted
#' @param stats a stats object summarizing the learning curve. As returned by \code{\link{lc.stats}}
#' @param show.fit whether to display the learning curve fit (defaults to \code{FALSE})
#' @param title a title for the plot, defaults to \code{\link{lc.title}}
#' @param focus.cutoff whether the plot should hide the noisy tail of the curve, as set by \code{stats$cutoff.msk}
#' @param show.n.students whether to display the population curve as well as the learning curve
#' @export
basic.learning.curve.plot <- function( sk, 
                                       stats, 
                                       show.fit=F, 
                                       title=lc.title(sk), 
                                       focus.cutoff=F, 
                                       show.se=T, 
                                       show.n.students=T )
{
  u.bound = NULL
  l.bound = NULL
  if( is.numeric(sk$slip) )
    u.bound = 100 * (1.0 - sk$slip)
  if( is.numeric(sk$slip) && is.numeric(sk$guess) && is.numeric(sk$init) )
    l.bound = 100 * (sk$init * (1.0 - sk$slip) + (1.0 - sk$init) * sk$guess)
  plot.learning.curve( stats$p.correct, 
                       stats$total, 
                       stats$se, 
                       sum(stats$cutoff.msk), 
                       if(show.fit){stats$fit}else{NULL}, 
                       title=title, 
                       focus.cutoff, 
                       !focus.cutoff, 
                       show.n.students,
                       u.bound,
                       l.bound )
}

#' Plot a learning curve
#' 
#' @param p.correct data of the curve to plot (percentage correct per opportunity)
#' @param total the total number of responses per opportunity
#' @param se the standard error to display around the p.correct curve
#' @param cutoff a logical vector masking out opportunities not to be displayed
#' @param fit an lm fit object specifying the fit to the curve
#' @param title the title to be displayed on the plot
#' @param focus.cutoff whether the plot should hide the noisy tail of the curve, as set by \code{stats$cutoff.msk}
#' @param draw.cutoff whether the cutoff line should be displayed
#' @param show.n.students whether to display the population curve as well as the learning curve
#' @param asymp.upper.bound display a dashed line for the expected asymptotic upper bound of the learning curve. 
#' If \code{NULL}, then no line is displayed.
#' @param asymp.lower.bound display a dashed line for the expected asymptotic lower bound of the learning curve
#' If \code{NULL}, then no line is displayed.
#' @export
plot.learning.curve <- function( p.correct,
                                 total,
                                 se,
                                 cutoff,
                                 fit=NULL,
                                 title="Learning Curve",                                  
                                 focus.cutoff=F, 
                                 draw.cutoff=T, 
                                 show.n.students=T,
                                 asymp.upper.bound=NULL,
                                 asymp.lower.bound=NULL )
{ 
  # maybe we should limit data to only some subset
  my.cutoff.msk = logical( length(p.correct) )
  if( ! focus.cutoff ){
    my.cutoff.msk[] = TRUE
  } else {
    my.cutoff.msk[1:cutoff] = TRUE
  }
  se.msk = ! is.na(se) & my.cutoff.msk
  
  # opportunity numbers
  opp.nums = 1:sum(my.cutoff.msk)
  
  # plot our graphs
  par(mar=c(5,4,4,2),oma=c(0,0,0,3))
  # make error bars
  if( ! is.null(se) ){
    buf=1.2
    bkwd = length(opp.nums):1
    lightblue = rgb( 0, 0, 1, 0.3 )
    #par(new=T)
    se.opp.nums = opp.nums[se.msk[opp.nums]]
    bkwd.opp.nums = bkwd[se.msk[bkwd]]
    pos.pts = p.correct[se.msk] + se[se.msk]
    neg.pts = p.correct[se.msk] - se[se.msk]
    plot( se.opp.nums, pos.pts,
          ylim=c(0,100), xlim=c(min(opp.nums),max(opp.nums)), type="l", col="blue", axes=F, ylab="", xlab="" )
    par(new=T)
    plot( se.opp.nums, neg.pts,
          ylim=c(0,100), xlim=c(min(opp.nums),max(opp.nums)), type="l", col="blue", axes=F, ylab="", xlab="" )
    polygon( c( se.opp.nums, bkwd.opp.nums ), 
             c( pos.pts, neg.pts[bkwd.opp.nums] ), 
             border=NA, col=lightblue, xlim=c(min(opp.nums),max(opp.nums)) ) 
    par(new=T)
  }
  # plot %correct
  plot( opp.nums, p.correct[my.cutoff.msk],
        ylim=c(0,100), type="b", col="blue", axes=F, ylab="", xlab="opportunities" )
  #axis(2,pretty(range(100*(correct/total),na.rm=T),10),col="blue")
  axis(2,pretty(c(0,100),10),col="blue")
  mtext("% correct",side=2,line=2,col="blue",outer=F)
  # plot asymptotic bounds
  if( is.numeric(asymp.upper.bound) )
    abline(h=asymp.upper.bound,lty=3)
  if( is.numeric(asymp.lower.bound) )
    abline(h=asymp.lower.bound,lty=3)
  # plot #students
  if( show.n.students ){
    par(new=T)
    plot( opp.nums, total[my.cutoff.msk], type="b", col="red", axes=F, ylab="", xlab="", pch=0 )
    rng = range(total[my.cutoff.msk],na.rm=T)
    if( rng[1] != rng[2] )
      axis(4,pretty(rng,10),col="red")
    mtext("# students",side=4,line=1,col="red",outer=T)
  }
  # x-axis, opportunities
  axis(1,opp.nums,col="black")
  mtext(title,side=3,col="black",outer=F)
  
  # show a line when we hit < 10% of students
  if( draw.cutoff ){  
    abline(v=cutoff,lty=2)
  }
  
  # show a power-law fit to the data
  if( ! is.null(fit) ){
    # maybe we need to re-calculate the fit
    fit.opps = 1:cutoff
    if( length(fit$fitted.values) == cutoff ){
      my.fit = fit
    } else {    
      my.fit = fit.learning.curve( NULL, total[fit.opps], p.correct[fit.opps] )
    }
    par(new=T)
    plot( fit.opps, (100 - exp(my.fit$fitted.values[fit.opps])), 
          axes=F, ylab="", xlab="", ylim=c(0,100), xlim=c(min(opp.nums),max(opp.nums)), type="b", col="black", pch=4 )
    legtxt = paste("% correct = 100 - ", 
                   prettyNum( exp(my.fit$coefficients[1]), digits=3), 
                   " * opportunity^", 
                   prettyNum( my.fit$coefficients[2], digits=3 ), 
                   "\nR^2 = ", 
                   prettyNum( summary(my.fit)$r.squared, digits=3), sep='')
    legend(x=1,y=10,legend=legtxt,box.lty=2)
  }
}
