# Description: Verifies that the vector R has been provided correctly.
# 
# Author: Linh Tran
# Date: May 14, 2015
###############################################################################


checkR = function(R, T, id) {
	if(length(unique(R))==1) stop("R needs to have two groups.")
	if(length(R)==length(T)) {
		R = tapply(R, id, function(x) x[1])
		return(R)
	} else {
		stop("Length of R needs to be same length as T.")
	}
}

