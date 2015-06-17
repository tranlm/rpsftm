# Description: Verifies that the vector C has been provided correctly.
# 
# Author: Linh Tran
# Date: May 14, 2015
###############################################################################


checkC = function(C, T, id) {
	if(length(C)==length(T)) {
		Ccount = tapply(C, id, function(x) length(unique(x)))
		if(max(Ccount)>1) {
			stop("Censored time variable (C) has more than 1 unique value across id observations.")
		} else {
			C = tapply(C, id, function(x) x[1])
		}
		return(C)
	} else if(length(C)==length(unique(id))) {
		if(!is.null(names(C))) {
			if(length(setdiff(unique(id), names(C)))==0) {
				return(C)
			}
		} else {
			stop("C has same length as unique values of id, but has not been assigned names of id.")
		}
	} else {
		stop("Length of C needs to be same length as either T or unique values of id.")
	}
}

