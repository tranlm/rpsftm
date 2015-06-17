# Description: Verifies that the vector Y has been provided correctly.
# 
# Author: Linh Tran
# Date: May 14, 2015
###############################################################################


checkY = function(Y, T, id) {
	## IF LONGER LENGTH ##
	if(length(Y)==length(T)) {
		Ycount = tapply(Y, id, function(x) length(unique(x)))
		if(max(Ycount)>1) {
			message("Y has more than 1 unique value across subject observations. Will assume time-varying")
			Y = tapply(Y, id, function(x) max(x, na.rm=TRUE))
			Y[Y==-Inf] = 0
		} else {
			Y = tapply(Y, id, function(x) x[1])
		}
		return(Y)
	} else if(length(Y)==length(unique(id))) {
		if(!is.null(names(Y))) {
			if(length(setdiff(unique(id), names(Y)))==0) {
				return(Y)
			}
		} else {
			stop("Y has same length as unique values of id, but has not been assigned names of id.")
		}
	} else {
		stop("Length of Y needs to be same length as T or unique values of id.")
	}
}

