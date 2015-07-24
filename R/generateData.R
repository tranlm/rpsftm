# Description: Generates simulation data
# 
# Author: Linh Tran
# Date: 04 May 2015
###############################################################################




#' Generating simulated clinical trial data
#' 
#' \code{generateData} generates observations which mimick data observed in a clinical trial.
#' 
#' This code will generate data based on an underlying baseline set of hazards, an effect value, and an assumed statistical model. In being fed the set of hazards, the code assumes that survival will follow a piecewise constant survival distribution (i.e. a piecewise exponential distribution). Possible treatment effects include an accelerated factor, proportional hazards, additive hazards, and log-linear declining relative hazard.
#' 
#' @param n The number of observations desired to generate 
#' @param lambda The set of hazards to be used in generating the data
#' @param time The number of time points to generate the data over
#' @param p The proportion of patients being assigned active treatment.
#' @param alpha The effect of the treatment, under the model specified. Under the \code{ldr} assumption, two values will have to be supplied: (1) an effect of treatment, and (2) a parity time point at which there is no longer a treatment effect.
#' @param txEffect The statistical model assumed for the treatment effect to be applied. \code{accel} will accelerate/decelerate the survival curve by \code{exp(alpha)}. \code{propor} will increase/decrease the hazard rates by a factor of \code{exp(alpha)}. \code{additive} will increase/decrease the hazard rates by adding \code{alpha}. \code{ldr} will increase/decrease the hazards by a factor of \code{exp(alpha)} which decreases over time until a parity time is met (at which point there is no treatment effect and hazard rates between the two treatment groups are equal). If the \code{ldr} treatment is specified, 
#' @param c proportion of subjects to be censored at each time point. Currently assumes censoring to be independent of survival time.
#' @param long logical; If \code{TRUE}, the data set returned will be in long format (as opposed to wide). 
#' @return An object of class \code{simData} will be returned, which includes both the data and the hazards used to generate the observations. 
#' 
#' @author Linh Tran
#' @export
generateData = function(n, lambda, time, p=0.5, alpha, txEffect="accel", c=0, crossover=0, long=FALSE) {

	############
	## CHECKS ##
	############
	if(length(time)==1) {
		time = seq.int(1,time)
	}
	if(length(lambda)==1) {
		lambda = rep(lambda, length(time))
	} else if(length(lambda) > length(time)) {
		warning("'lambda' has a longer length than 'time'. Will only use up to end time.")
		lambda = lambda[1:length(time)]
	} else if(length(lambda) < length(time)) {
		warning("'lambda' has a shorter length than 'time'. Will carry forward last value.")
		lambda[length(lambda):length(time)] = lambda[length(lambda)]
	}
	
	#################
	## INITIALIZES ##
	#################
	var.names = NULL
	for(i in 1:length(time)) {
		var.names = c(var.names, paste(c("L","A","C","Y"), i, sep="."))
	}
	LT = matrix(nrow=n, ncol=length(var.names)+3, dimnames=list(NULL, c("W1", "W2", var.names, "delta")))
	Leff = 0.2
	
	############
	## TIME 1 ##
	############
	atrisk = rep(TRUE,n)
	LT[atrisk,"W1"] = rbinom(n, 1, p=.7)
	LT[atrisk,"W2"] = rnorm(n)
	LT[atrisk,"L.1"] = rnorm(n)
	center = rep(mean(LT[atrisk,"L.1"]),n)
	LT[atrisk,"A.1"] = rbinom(n, 1, prob=p)
#	LT[atrisk,"A.1"] = 0
	lambda.tx = txLambda(lambda, time, alpha, type=txEffect)
	lambdaOBS = t(apply(LT[,"A.1", drop=FALSE], 1, function(x) {
			if(x==0) return(lambda)
			else return(lambda.tx)
		}))
	colnames(lambdaOBS) = paste("lambda", c(0:(length(lambda)-1)), sep=".")
	lambdaOBS[,1] = plogis(qlogis(lambdaOBS[,1]) + Leff*LT[atrisk,"L.1"] - Leff*center)
	LT[atrisk,"C.1"] = rbinom(n, 1, prob=c)	## INDEP CENSORING FOR NOW
	atrisk = atrisk & LT[atrisk,"C.1"]==0
	LT[,"Y.1"] = ifelse(atrisk, rbinom(n, 1, prob=1-exp(-lambdaOBS[,1])), NA)

	## TIMES 2 ONWARDS ##
	if(max(time)>1) {
		for(i in 2:max(time)) {
			## AT RISK INDICATOR ##
			atrisk = atrisk & (LT[,paste("C.",i-1, sep="")]==0 & LT[,paste("Y.",i-1, sep="")]==0)
			
			## TIME-DEPENDENT CONFOUNDER
			ibeta = 0.25
			Lbeta = 0.25
			e = rnorm(n)
			LT[,paste("L.",i, sep="")] = ifelse(atrisk, ibeta + Lbeta*LT[,paste("L.",i-1, sep="")] - 0.5*LT[,paste("A.",i-1, sep="")] + e, NA)
			center = ifelse(atrisk, ifelse(LT[,paste("A.",i-1, sep="")]==1, -ibeta , ibeta ) + Lbeta*center + mean(e[atrisk]), NA)
			
			## TREATMENT ##
			if(crossover>0) {
				prob = plogis(qlogis(crossover) + 0.7*LT[atrisk,paste("L.",i, sep="")])
				LT[atrisk, paste("A.",i, sep="")] = ifelse(LT[atrisk, paste("A.",i-1, sep="")]==1, 1, rbinom(rep(1,sum(atrisk)), 1, prob=prob))
			} else {
				LT[atrisk, paste("A.",i, sep="")] = LT[atrisk, "A.1"]
			}
#			LT[atrisk, paste("A.",i, sep="")] = ifelse(i<25, 0, 1)
			
			## HAZARD ##
			mitigation = 0.75
			lambda.tx = txLambda(lambda[i:length(lambda)], time[i:length(time)], alpha*mitigation, type=txEffect)
			crossed = atrisk & LT[, paste("A.",i-1, sep="")] == 0 & LT[, paste("A.",i, sep="")] == 1
			if(sum(crossed)>0) {
				lambdaOBS[crossed,i:max(time)] = t(matrix(lambda.tx, nrow=length(lambda.tx), ncol=sum(crossed)))			
			}
			lambdaOBS[atrisk,i] = plogis(qlogis(lambdaOBS[atrisk,i]) + Leff*LT[atrisk,paste("L.",i, sep="")] - Leff*center[atrisk])
			
			## CENSORING ##
			LT[, paste("C.",i, sep="")] = ifelse(atrisk, rbinom(rep(1,n), 1, prob=c), NA)	## INDEP CENSORING FOR NOW
			atrisk = atrisk & LT[, paste("C.",i, sep="")]==0
			
			## DEATH INDICATOR ##
			LT[, paste("Y.",i, sep="")] = suppressWarnings(ifelse(atrisk, rbinom(rep(1,n), 1, prob=1-exp(-lambdaOBS[,i])), NA))
		}
	}
	
	## SURVIVAL TIMES ##
	tmp = suppressWarnings(apply(LT[,paste("Y.", 1:max(time), sep=""), drop=FALSE], 1, function(x) max(x, na.rm=TRUE)))
	LT[, "delta"] = ifelse(tmp==-Inf, 0, tmp)
	T = apply(LT[, paste("Y.", 1:max(time), sep=""), drop=FALSE], 1, function(x) sum(1-x, na.rm=TRUE)+1)
	data = data.frame(LT, T)

	## TRANSPOSE ##
	if(long) {
		data = reshape(data, varying=c(3:(ncol(data)-2)), direction="long")
		data = subset(data, !(is.na(L) & is.na(A) & is.na(C) & is.na(Y)))
		data = data[order(data$id, data$time),]
		rownames(data) = NULL
	} else {
		data$id = rownames(data)
	}
	
	foo = list(data=data, lambdaOBS=lambdaOBS, lambda=lambda, time=time, alpha=alpha, txEffect=txEffect, long=long)
	class(foo) = "simData"
	
	return(foo)
}
