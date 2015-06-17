# Description: Verifies that the vector R has been provided correctly.
# 
# Author: Linh Tran
# Date: May 14, 2015
###############################################################################


checkR = function(R, T, id) {
	if(length(R)==length(T)) {
		Rcount = tapply(R, id, function(x) length(unique(x)))
		if(max(Rcount)>1) {
			stop("Assigned treatment (R) has more than 1 unique value across id observations.")
		} else {
			R = tapply(R, id, function(x) x[1])
		}
		return(R)
	} else if(length(R)==length(unique(id))) {
		if(!is.null(names(R))) {
			if(length(setdiff(unique(id), names(R)))==0) {
				return(R)
			}
		} else {
			stop("R has same length as unique values of id, but has not been assigned names of id.")
		}
	} else {
		stop("Length of R needs to be same length as either T or unique values of id.")
	}
}

