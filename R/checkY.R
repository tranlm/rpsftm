# Description: Verifies that the vector Y has been provided correctly.
# 
# Author: Linh Tran
# Date: May 14, 2015
###############################################################################


checkY = function(Y, T, id) {
	## IF LONGER LENGTH ##
	if(length(Y)==length(T)) {
		Y = tapply(Y, id, function(x) x[1])
		return(Y)
	} else {
		stop("Length of Y needs to be same length as T.")
	}
}

