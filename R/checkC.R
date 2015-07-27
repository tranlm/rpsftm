# Description: Verifies that the vector C has been provided correctly.
# 
# Author: Linh Tran
# Date: May 14, 2015
###############################################################################


checkC = function(C, T, id) {
	if(length(C)==length(T)) {
		C = tapply(C, id, function(x) x[1])
		return(C)
	} else {
		stop("Length of C needs to be same length as T.")
	}
}

