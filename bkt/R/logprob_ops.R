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

################################################
# Functions to help with operations in log-space

# this is an odd situation where you've got log(a), and log(b)
# and what you want is log(a+b), and sometimes this numerically explodes
# to Inf, so we need to be careful
logadd = function( la, lb )
{
	if( is.infinite(la) )
		return(lb)
	else if( is.infinite(lb) )
		return(la)
	if( la > lb )
		return( la + log(1+exp(lb-la)) )
	lb + log(1+exp(la-lb))
}

logsum = function( logs )
{
	s = logs[1]
	if( length(logs) > 1 )
		for( i in 2:length(logs) )
			s = logadd( s, logs[i] )
	s
}

# trying to replicate the functionality of logadd on matrices
# assumes the two matrices have the same dimensions
mlogadd <- function( mla, mlb )
{
	if( is.matrix(mla) )
		added = matrix(data=-Inf,nrow=nrow(mla),ncol=ncol(mla))
	else
		added = rep(-Inf,length(mla))
	a.inf = is.infinite(mla)
	b.inf = is.infinite(mlb)
	added[ a.inf ] = mlb[ a.inf ]
	added[ b.inf ] = mla[ b.inf ]
	n.inf = ! a.inf & ! b.inf
	a.msk = mla[n.inf] > mlb[n.inf]
	added[n.inf][a.msk] = mla[n.inf][a.msk] + log(1+exp(mlb[n.inf][a.msk] - mla[n.inf][a.msk]))
	added[n.inf][!a.msk] = mlb[n.inf][!a.msk] + log(1+exp(mla[n.inf][!a.msk] - mlb[n.inf][!a.msk]))
	return(added)
}

