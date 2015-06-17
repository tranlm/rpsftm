# Description: Reads in Life Table and computes year specific mortality rates
# 
# Author: Linh Tran
# Date: May 4, 2015
###############################################################################

#' @export 
USmx = function(file) {
	LT = read.xlsx2(file, 1, startRow=7, colIndex=c(3,7), header=TRUE, stringsAsFactors=FALSE)
	LT$l.x. = as.numeric(LT$l.x.)
	mx = rep(NA, nrow(LT))
	mx[1:(nrow(LT)-1)] = -log(LT$l.x.[2:nrow(LT)] / LT$l.x.[1:(nrow(LT)-1)])
	mx[nrow(LT)] = 1/LT$e.x.[nrow(LT)]
	LT$m.x. = mx
	return(LT)
}

