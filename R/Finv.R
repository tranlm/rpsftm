# Description: Function to get inverse of F
# 
# Author: Linh Tran
# Date: May 8, 2015
###############################################################################


Finv = function(p, F, x) {
	
	## CHECKS ##
	if(length(F)!=length(x)) stop("F and x need to be same length")
	if(any(p>1) | any(p<0)) stop("p needs to be within [0,1]")
	index.ordered = order(x)
	F = F[index.ordered]
	x = x[index.ordered]
	if(any(cummax(F)<F)) stop("The cdf is not monotone")
	unique.xs = unique(x)
	if(length(unique.xs) != length(x)) {
		unique.obs = unique(cbind(F,x))
		if(nrow(unique.obs)!=length(x)) stop("Each x has to have a unique CDF value")
	}
	if(length(p)>length(x)) warning("More values of p being requested than data supports")

	values = rep(NA, length(p))
	for(i in 1:length(p)) {
		index = which(F>=p[i])
		if(length(index)>0) values[i] = x[min(index)]
		else values[i] = 1
		index = NULL
	}
	return(values)
}


