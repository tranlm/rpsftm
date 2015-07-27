# Description: Utility functions to help package
# 
# Author: Linh Tran
# Date: May 14, 2015
###############################################################################


PrintCall <- function(cl) {
	cat("Call:\n", paste(deparse(cl), sep = "\n", collapse = "\n"), "\n\n", sep = "")
}


#' @export 
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


#' @export 
print.rpsftm = function(x, ...) {
	PrintCall(x$call)
	cat("RPSFTM Estimates (Model based 95% CI)\n\n")
	results = cbind(x$estimates, x$ci)
	colnames(results)[1] = "Psi Est."
	print(results)
	
	invisible(x)
}


#' @export 
plot.rpsftm = function(fit, statistic="Log-rank", ...) {
	if(!statistic %in% c("Log-rank", "Wilcoxon", "Cox-Wald", "Cox-HR")) stop("statistic needs to be one of the following: Log-rank, Wilcoxon, Cox-Wald, Cox-HR")
	
	psi = fit$statistics$psi
	if(statistic=="Log-rank") {
		pvalue = "km.pvalue"
		ci = "km.ci"
	}
	else if(statistic=="Wilcoxon") {
		pvalue = "wilcoxon.pvalue"
		ci = "wil.ci"
	}
	else if (statistic=="Cox-Wald") {
		pvalue = "cox.wald.pvalue"
		ci = "cox.wald.ci"
	}
	else if (statistic=="Cox-HR") {
		pvalue = "cox.HR.pvalue"
		ci = "cox.hr.ci"
	}
	
	plot(NULL, xlim=c(min(psi),max(psi)), ylim=c(0,1), xlab=expression(psi), ylab="p-value", main=paste("p-values using", statistic, "statistic"), yaxt="n", xaxt="n")
	rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="gray90")
	abline(v=seq(min(psi),max(psi),diff(range(psi))/5), col="white", lwd=1.5)
	abline(v=seq(min(psi),max(psi),diff(range(psi))/10), col="white", lwd=.75)
	abline(h=seq(0,1,.2), col="white", lwd=1.5)
	abline(h=seq(0,1,.1), col="white", lwd=.75)
	axis(1, seq(min(psi),max(psi),diff(range(psi))/5), labels=TRUE, las=1, tcl=-.25)
	axis(1, seq(min(psi),max(psi),diff(range(psi))/10), labels=FALSE, las=1, tcl=-.25)
	axis(2, seq(0,1,.2), labels=TRUE, las=1)
	axis(2, seq(0,1,.1), labels=FALSE, las=1, tcl=-.25)
	
	lines(fit$statistics[,pvalue] ~ fit$statistics$psi, lwd=2)
	abline(h=.05, lty=2, col=2, lwd=2)
	abline(v=fit$ci[ci,1], lwd=2, col=3, lty=2)
	abline(v=fit$ci[ci,2], lwd=2, col=3, lty=2)
	mtext("0.05", 4, at=.06, las=1, line=.25)
			
}


