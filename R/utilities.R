# Description: Utility functions to help package
# 
# Author: Linh Tran
# Date: May 14, 2015
###############################################################################


PrintCall <- function(cl) {
	cat("Call:\n", paste(deparse(cl), sep = "\n", collapse = "\n"), "\n\n", sep = "")
}


print.simData = function(x, ...) {
	cat("n =", length(unique(x$data$id)), "\n")
	cat("Treatment effect:", x$txEffect, "\n")
	cat("Treatment value:", x$alpha, "\n")
	cat("Time points:", max(x$time), "\n")
	if(x$long) {
		cat("Crossovers:", sum(with(x$data, tapply(A, id, function(x) length(unique(x))==2))), "\n")
		data = subset(x$data, time==1)
		cat("Deaths:", sum(data$delta), "\n")
		cat("Survival times:\n")
		print(summary(data$T))
	} else {
		cat("Crossovers:", sum(apply(x$data[,grep("A.",names(x$data))], 1, function(x) length(unique(x))==2)), "\n")
		cat("Deaths:", sum(x$data$delta), "\n")
		cat("Survival times:\n")
		print(summary(x$data$T))
	}
	invisible(x)
}


print.rpsftm = function(x, ...) {
	PrintCall(x$call)
	cat("RPSFTM Estimates (Model based 95% CI)\n\n")
	results = cbind(x$estimates, x$ci)
	colnames(results)[1] = "Psi Est."
	print(results)
	
	invisible(x)
}


