# Description: Returns the date of analysis for a simulated data set, based on number of events desired
# 
# Author: Linh Tran
# Date: May 13, 2015
###############################################################################


#' @export
getAnalysisDate = function(dateEnd, delta, events) {
	endOrder = order(dateEnd)
	dateEnd = dateEnd[endOrder]
	eventCount = cumsum(delta[endOrder])
	analysisDate = max(which(eventCount<=events))+1
	if(analysisDate>length(dateEnd)) {
		warning("Number of events is less or equal to desired amount.")
		analysisDate = length(dateEnd)
	}		
	return(dateEnd[analysisDate])
}

