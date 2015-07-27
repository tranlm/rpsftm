# Description: Verifies that the vector strata has been provided correctly.
# 
# Author: Linh Tran
# Date: May 14, 2015
###############################################################################


checkStrata = function(strata, T, id) {
	if(is.null(strata)) {
		strata = factor(rep(1,length(T)))
		names(strata) = id
	}
	if(!is.factor(strata)) stop("Strata needs to be provided as a factor.")
	if(length(strata)==length(T)) {
		strata = tapply(strata, id, function(x) x[1])
	} else {
		stop("Length of strata needs to be same length as T.")
	}
	return(strata)
}

