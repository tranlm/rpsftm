# Description: Verifies that the vector strata has been provided correctly.
# 
# Author: Linh Tran
# Date: May 14, 2015
###############################################################################


checkStrata = function(strata, T, id) {
	if(is.null(strata)) return(factor(rep(1,length(unique(id)))))
	if(!is.factor(strata)) stop("Strata needs to be provided as a factor.")
	if(length(strata)==length(T)) {
		stratacount = tapply(strata, id, function(x) length(unique(x)))
		if(max(stratacount)>1) {
			stop("Strata has more than 1 unique value across id observations.")
		} else {
			strata = tapply(strata, id, function(x) x[1])
		}
		return(strata)
	} else if(length(strata)==length(unique(id))) {
		if(!is.null(names(strata))) {
			if(length(setdiff(unique(id), names(strata)))==0) {
				return(strata)
			}
		} else {
			stop("Strata has same length as unique values of id, but has not been assigned names of id.")
		}
	} else {
		stop("Length of strata needs to be same length as either T or unique values of id.")
	}
}

