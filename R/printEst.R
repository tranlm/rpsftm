# Description: Add comment
# 
# Author: Linh Tran
# Date: Jun 18, 2015
###############################################################################


#' @export
printEst = function(results, est, se, beta) {
	cat("###########################\n")
	cat("########", est, "########\n")
	cat("###########################\n")
	cat("Bias:\t\t", mean(results[,est]) - beta, "\n")
	cat("MSE:\t\t", mean((results[,est] - beta)^2), "\n")
	cat("Var:\t\t", var(results[,est]), "\n")
	cat("sqrt(Var):\t", sqrt(var(results[,est])), "\n")
	cat("mean SE:\t", mean(results[,se]), "\n")
	lcl = results[,est] - qnorm(.975)*results[,se]
	ucl = results[,est] + qnorm(.975)*results[,se]
	cat("CI length:\t", mean(ucl-lcl), "\n")
	cat("Coverage:\t", mean(ifelse(lcl <= beta & beta <= ucl, 1, 0)), "\n")
}

