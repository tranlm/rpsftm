# Description: Verifies that the vector weights have been provided correctly.
# 
# Author: Linh Tran
# Date: June 11, 2015
###############################################################################



checkWeights = function(weights, T, id) {
	if(is.null(weights)) {
		weights = rep(1, length(unique(id)))
		names(weights) = unique(id)
		return(weights)
	}
	if(any(is.na(weights))) stop("Weight vector cannot have NA values.")
	## IF LONGER LENGTH ##
	if(length(weights)==length(T)) {
		cum.wt = unlist(tapply(weights, id, prod))
		if(min(cum.wt)<=0) {
			stop("Weights cannot have values less than or equal to 0.")
		}
		if(any(weights>1)) warning("Weights can only be used in the Cox Model. Log-rank and Wilcoxon statistic will be calculated with no weights.")
		return(cum.wt)
	} else {
		stop("Length of weights needs to be same length as T or unique values of id.")
	}
}

