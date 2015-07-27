# Description: Code to implement RPSFTM
# 
# Author: Linh Tran
# Date: May 6, 2015
###############################################################################



#' Rank Preserving Structural Failure Time Model
#' 
#' \code{rpsftm} implements the rank preserving structural failure time model presented by Robins and Tsiatis (1991) and again by Korhonen et al (2012).
#' 
#' Data should be provided to the function in a longitudinal format, with the indicator of final outcome \code{Y} being constant across all observations from the same individual. \code{T} is the interval specific time contributed while on the specific treatment \code{TX}. 
#' 
#' @param T Total follow-up time contributed by each patient. This is typically the min(T,C), where T is the event time and C is the censoring time. 
#' @param A Total time each subject spends on active treatment. For subjects who are never exposed to active treatment (i.e. they are randomized to and remain in the placebo group), this value will be 0. For subjects remaining in the active treatment group during their entire follow-up, this will be equal to \code{T}.
#' @param R Indicator of treatment group initially randomized to. This is a binary variable that takes on value 1 if the subject was randomized to treatment and 0 if the subject was randomized to the control group.
#' @param C The observed censoring time. The model assumes that if a subject experiences a failure prior to observing the censoring time, then the censoring time would be the time from randomization until the end of study. It also assumes that censoring is independent of treatment assignment \code{R}. Typically, this is the indicator that min(T,C) is equal to T, i.e. that a failure was observed before a censoring event.
#' @param Y Indicator of observed failure event. This is a binary variable that takes on value 1 if the event is observed and 0 otherwise. N 
#' @param id Cluster identification variable. This is used to label the counterfactual survival times.
#' @param strata Strata that each id is assigned to. 
#' @param psi Grid of potential parameter values to estimate over. By default, the grid is set to \code{seq(-3,3,.001)}.
#' @param weight Observational weights.
#' @return From the potential values of psi, the function will return an object of class \code{rpfstm}, which includes (1) the statistics for the counterfactual distribution, (2) the empirical values for the counterfactual distribution off treatment, and (3) the summarized data for time spent both on and off treatment. The summarized data returned is ordered by \code{id}, with numeric IDs converted to character form. 
#' 
#' For the statistics, the following values are reported
#' \itemize{
#' 	\item {log-rank statistic from the Kaplan Meier curve}
#' 	\item {log-rank p-value from the Kaplan Meier curve}
#' 	\item {Wilcoxon statistic from the Kaplan Meier curve}
#' 	\item {Wilcoxon p-value from the Kaplan Meier curve}
#' 	\item {Cox Wald statistic from the proportional hazards model}
#' 	\item {Cox Wald p-value from the proportional hazards model}
#' 	\item {Cox likelihood ratio test statistic from the proportional hazards model}
#' 	\item {Cox likelihood ratio test p-value from the proportional hazards model}
#' 	\item {Cox hazard ratio from the proportional hazards model}
#' 	\item {Cox hazard ratio z-statistic from the proportional hazards model}
#' 	\item {Cox hazard ratio p-value from the proportional hazards model}
#' }
#' 
#' For the counterfactual distribution off treatment
#' \itemize{
#' 	\item {log-rank statistic from the Kaplan Meier curve}
#' 	\item {log-rank p-value from the Kaplan Meier curve}
#' 	\item {Wilcoxon statistic from the Kaplan Meier curve}
#' 	\item {Wilcoxon p-value from the Kaplan Meier curve}
#' 	\item {Cox Wald statistic from the proportional hazards model}
#' 	\item {Cox Wald p-value from the proportional hazards model}
#' 	\item {Cox likelihood ratio test statistic from the proportional hazards model}
#' 	\item {Cox likelihood ratio test p-value from the proportional hazards model}
#' 	\item {Cox hazard ratio from the proportional hazards model}
#' 	\item {Cox hazard ratio z-statistic from the proportional hazards model}
#' 	\item {Cox hazard ratio p-value from the proportional hazards model}
#' }
#' 
#' @examples
#' # Simulated data under no crossover or censoring
#' set.seed(1)
#' n = 100
#' R = rbinom(n, 1, 0.5)
#' T0 = rexp(n, .05)
#' T = ifelse(R==0, T0, exp(0.2)*T0)
#' A = ifelse(R==0, 0, T)
#' rpsftmFit1 = rpsftm(T, A, R, C=rep(Inf, n), Y=rep(1,n), id=seq(1,n), psi=seq(-3,3,.01))
#' rpsftmFit1
#' plot(rpsftmFit1)
#'
#' # Simulated data under placebo crossover halfway through follow-up (no censoring)
#' set.seed(1)
#' n = 100
#' R = rbinom(n, 1, 0.5)
#' crossover = ifelse(R==1, 0, rbinom(n, 1, 0.5))
#' T0 = rexp(n, .05)
#' T = ifelse(R==0, ifelse(crossover==1, T0/2+exp(0.2)*(T0/2), T0), exp(0.2)*T0)
#' A = ifelse(R==0, ifelse(crossover==1, exp(0.2)*(T0/2), 0), T)
#' rpsftmFit2 = rpsftm(T, A, R, C=rep(Inf, n), Y=rep(1,n), id=seq(1,n), psi=seq(-3,3,.01))
#' rpsftmFit2
#' plot(rpsftmFit2)
#'
#' @author Linh Tran
#' @export
rpsftm = function(T, A, R, C, Y, id, strata=NULL, psi=seq(-3,3,.001), weights=NULL) {
	
	## INITIALIZES ##
	psi = psi[order(psi)]
	
	## CHECKS ##
	if(length(unique(id)) != length(id)) stop("IDs supplied need to be unique.")
	if(length(T)!=length(A)) stop("Length of T and A vectors need to be identical.")
	if(length(T)!=length(id)) stop("Length of id and A/T vectors need to be identical.")
	R = checkR(R, T, id)
	C = checkC(C, T, id)
	Y = checkY(Y, T, id)
	strata = checkStrata(strata, T, id)
	weights = checkWeights(weights, T, id)
	
	
	## FINAL DATA ##
	data = matrix(nrow=length(Y), ncol=7, dimnames=list(id, c("R", "Y", "C", "strata", "T_off", "T_on", "weights")))
	data[names(Y),"Y"] = Y
	data[names(C),"C"] = C
	data[names(R),"R"] = R
	data[names(strata), "strata"] = strata
	data[id,"T_off"] = T-A
	data[id,"T_on"] = A	
	data[names(weights),"weights"] = weights
	
	## ESTIMATES STATISTIC FROM PSI ##
	results = lapply(1:length(psi), function(i) {
		
		# Counterfactual time off treatment
		U_psi = data[,"T_off"] + exp(psi[i])*data[,"T_on"]

		# Censoring
		C_psi = pmin(data[,"C"], exp(psi[i])*data[,"C"])
		X_psi = pmin(U_psi, C_psi)
		Y_psi = ifelse(data[,"Y"]==1, 1*I(X_psi==U_psi), 0)
		
		# Survival
		survObject = Surv(X_psi, Y_psi)
		
		# Log-rank
		kmFit = survdiff(survObject ~ data[,"R"] + strata(data[,"strata"]))
		
		# Wilcoxon
		wilcoxonFit = survdiff(survObject ~ data[,"R"] + strata(data[,"strata"]), rho=1)
		
		# Cox prop hazards
		coxFit = coxph(survObject ~ data[,"R"] + strata(data[,"strata"]), weights=weights)
		coxScore = resid(coxFit, type="score")
		controlScore = ifelse(data[,"R"]==0, coxScore, NA)

		statistics=c(psi=psi[i], km.chisq=kmFit$chisq, km.pvalue=1-pchisq(kmFit$chisq,1), wilcoxon.chisq=wilcoxonFit$chisq, wilcoxon.pvalue=1-pchisq(wilcoxonFit$chisq,1), cox.wald=coxFit$wald.test[[1]], cox.wald.pvalue=1-pchisq(coxFit$wald.test[[1]],1), cox.lrt=2*diff(coxFit$loglik), cox.lrt.pvalue=1-pchisq(2*diff(coxFit$loglik),1), cox.HR=coxFit$coefficients[[1]], cox.HR.z = summary(coxFit)$coef[,"z"], cox.HR.pvalue = summary(coxFit)$coef[,"Pr(>|z|)"])
		output = list(statistics=statistics, U_psi=U_psi, C_psi=C_psi, X_psi=X_psi, Y_psi=Y_psi)
		return(output)
	})

	## SUMMARIZES RESULTS ##
	stats = data.frame(do.call("rbind", lapply(results, function(x) return(x$statistics))))	
	U_psi = data.frame(do.call("cbind", lapply(results, function(x) return(x$U_psi))))
	C_psi = data.frame(do.call("cbind", lapply(results, function(x) return(x$C_psi))))
	X_psi = data.frame(do.call("cbind", lapply(results, function(x) return(x$X_psi))))
	Y_psi = data.frame(do.call("cbind", lapply(results, function(x) return(x$Y_psi))))
	counterfact = list(U_psi=U_psi, C_psi=C_psi, X_psi=X_psi, Y_psi=Y_psi)
	names(counterfact$U_psi) = names(counterfact$C_psi) = names(counterfact$X_psi) = names(counterfact$Y_psi) = paste("psi_", psi, sep="")
	out = list(statistics=stats, counterfact=counterfact, data=data)
	out$call <- match.call()
	out$estimates = c(stats$psi[which.max(stats$km.pvalue)], stats$psi[which.max(stats$wilcoxon.pvalue)], stats$psi[which.max(stats$cox.wald.pvalue)], stats$psi[which.min(stats$cox.HR^2)])	
	names(out$estimates) = c("Log-rank", "Wilcoxon", "Cox-Wald", "Cox-HR")
	if(any(out$estimates==max(stats$psi)) | any(out$estimates==min(stats$psi))) warning("Estimates are at boundaries of psi grid values. Recommended to widen the psi range.")
	km.ci = wil.ci = cox.wald.ci = cox.hr.ci = NULL
	km.i = which(stats$km.chisq<qchisq(.95,1))
	if(length(km.i)>0) {
		km.i = c(min(km.i)-1, max(km.i)+1)
		km.ci = c(lcl = psi[max(1,km.i[1])], ucl = psi[min(km.i[2],length(psi))])
	}	
	wil.i = which(stats$wilcoxon.chisq<qchisq(.95,1))
	if(length(wil.i)>0) {
		wil.i = c(min(wil.i)-1, max(wil.i)+1)
		wil.ci = c(lcl = psi[max(1,wil.i[1])], ucl = psi[min(wil.i[2],length(psi))])
	}
	cox.wald.i = which(stats$cox.wald<qchisq(.95,1))
	if(length(cox.wald.i)>0) {
		cox.wald.i = c(min(cox.wald.i)-1, max(cox.wald.i)+1)
		cox.wald.ci = c(lcl = psi[max(1,cox.wald.i[1])], ucl = psi[min(cox.wald.i[2],length(psi))])
	}

	cox.hr.i = which(abs(stats$cox.HR.z)<qnorm(.975))
	if(length(cox.hr.i)>0) {
		cox.hr.i = c(min(cox.hr.i)-1, max(cox.hr.i)+1)
		cox.hr.ci = c(lcl = psi[max(1,cox.hr.i[1])], ucl = psi[min(cox.hr.i[2],length(psi))])
	}
	out$ci = rbind(km.ci, wil.ci, cox.wald.ci, cox.hr.ci)
	if(is.null(out$ci)) out$ci = "Psi grid not large enough to compute confidence intervals."
	class(out) = "rpsftm"
	
	return(out)
	
}
