# Description: Verifies that the vector weights have been provided correctly.
# 
# Author: Linh Tran
# Date: June 11, 2015
###############################################################################



checkWeights = function(weights, T, id) {
	if(is.null(weights)) return(rep(1, length(unique(id))))
	if(any(is.na(weights))) stop("Weight vector cannot have NA values.")
	## IF LONGER LENGTH ##
	if(length(weights)==length(T)) {
		cum.wt = unlist(tapply(weights, id, prod))
		if(min(cum.wt)<=0) {
			stop("Weights cannot have values less than or equal to 0.")
		}
		if(any(weights>1)) warning("Weights can only be used in the Cox Model.")
		return(cum.wt)
	} else if(length(weights)==length(unique(id))) {
		if(!is.null(names(weights))) {
			if(length(setdiff(unique(id), names(weights)))==0) {
				if(all(weights>=0)) {
					if(any(weights)>1) warning("Weights can only be used in the Cox Model.")
					return(weights[unique(id)])
				} else {
					stop("Weights cannot have values less than or equal to 0.")
				}
			}
		} else {
			stop("Weights have same length as unique values of id, but has not been assigned names of id.")
		}
	} else {
		stop("Length of weights needs to be same length as T or unique values of id.")
	}
}

