# Description: Connie's version of the RPSFTM approach
# 
# Author: Linh Tran
# Date: May 12, 2015
###############################################################################

#' Connie's Rank Preserving Structural Failure Time Model
#' 
#' \code{crpsftm} implements the rank preserving structural failure time model presented by Robins and Tsiatis (1991) and again by Korhonen et al (2012).
#' 
#' Data should be provided in her format. 
#' 
#' @param data Data in her format. 
#' @param si Grid of potential parameter values to estimate over. By default, the grid is set to \code{seq(-3,3,.001)}. 
#' @return From the potential values of psi, the function will return a list with her preferred items.
#' 
#' @author Linh Tran
#' @export
crpsftm = function(data, si) {
	N=dim(data)[1]
	
	RPSFT000=function(data, si){		
		Z=data[1]
		futime=data[2]
		TALT=data[3]
		T=data[4]
		C=data[5]
		ALT=data[6]
		D=data[7]
		DISC=data[8]
		strata=data[9]
		U_True=data[10]
		
		X0_si=T
		CEN0_si=C
				
		T_ON=-1
		T_OFF=-1  
		
		U_ON=-1
		U=-1
		CEN_si=-1
		
		if(Z==1 )        {T_ON=T 
			T_OFF=0} 
		
		if(Z==0 & ALT==1){T_ON=T-TALT
			T_OFF=TALT} 
		
		if(Z==0 & ALT==0){T_ON=0
			T_OFF=T} 
		
		U_ON=T_ON*si
		U=U_ON+T_OFF
		C_si=min(futime, futime*si)
				
		if (C==0){
			X_si=min(T, T*si, C_si)
			CEN_si=0}
		
		if (C==1 & C_si >= U){
			X_si=U
			CEN_si=1}
				
		if (C==1 & C_si < U){
			X_si=C_si
			CEN_si=0}		
		
		if (Z==0 & ALT==1){
			X0_si=X_si
			CEN0_si=CEN_si}
		
		return (c(Z,futime,T,C,D, DISC,TALT,ALT,T_ON,T_OFF, U_ON,U, C_si,X_si,CEN_si,X0_si,CEN0_si,strata,U_True))		
	}
	
	exp_si=exp(si)
	length_si=length(si)
	
	#counterfactual survival time for both treatment arm and control arm
	X_si=matrix(-1,nrow=N,ncol=length_si)
	U_si=matrix(-1,nrow=N,ncol=length_si)
	C_si=matrix(-1,nrow=N,ncol=length_si)
	CEN_si=matrix(-1,nrow=N,ncol=length_si)
	
	chi_si=rep(-1,length_si)
	p_si=rep(-1,length_si)
	
	WCchi_si=rep(-1,length_si)
	WCp_si=rep(-1,length_si)
	
	coxHR_si=rep(-1,length_si)
	cox_p_si=rep(-1,length_si)
	
	#counterfactual survival time used for final analysis (Adjust for patients who received alternate therapy/crossover only
	X0_si=matrix(-1,nrow=N,ncol=length_si)
	CEN0_si=matrix(-1,nrow=N,ncol=length_si)
		
	for (k in 1:length_si){

		test_result=apply(data,1,RPSFT000, si=exp_si[k])	
		
		r_Z=test_result[1,]
		r_futime=test_result[2,]
		r_T=test_result[3,]
		r_C=test_result[4,]
		r_D=test_result[5,]
		r_DISC=test_result[6,]
		r_TALT=test_result[7,]
		r_ALT=test_result[8,]
		r_T_ON=test_result[9,]
		r_T_OFF=test_result[10,]
		r_U_ON=test_result[11,]
		r_U=test_result[12,]
		r_C_si=test_result[13,]
		r_X_si=test_result[14,]
		r_CEN_si=test_result[15,]
		r_X0_si=test_result[16,]
		r_CEN0_si=test_result[17,]
		r_strata=test_result[18,]
		r_U_True=test_result[19,]
		
		index_active=which(r_Z==1)
		index_control=which(r_Z==0)
				
		logrank11_si=survdiff( Surv(r_X_si, r_CEN_si) ~ r_Z+strata(r_strata))
		
		LRchi11_si=logrank11_si$chisq
		LRp11_si=1 - pchisq(logrank11_si$chisq, 1)
		
		Wilcoxon_si=survdiff( Surv(r_X_si, r_CEN_si) ~ r_Z+strata(r_strata),rho=1)
		
		Wilcoxonchi_si=Wilcoxon_si$chisq
		Wilcoxonp_si=1 - pchisq(Wilcoxon_si$chisq, 1)
				
		pvalue_cox_si=summary(coxph(Surv(r_X_si,r_CEN_si)~r_Z+strata(r_strata)))$waldtest[3]
		HR_cox_si=summary(coxph(Surv(r_X_si,r_CEN_si)~r_Z+strata(r_strata)))$coefficients[2]
		
		X_si[,k]=r_X_si		
		U_si[,k]=r_U
		C_si[,k]=r_C_si
		CEN_si[,k]=r_CEN_si
		
		X0_si[,k]=r_X0_si
		CEN0_si[,k]=r_CEN0_si
		
		chi_si[k]=LRchi11_si
		p_si[k]=LRp11_si
		
		WCchi_si[k]=Wilcoxonchi_si
		WCp_si[k]=Wilcoxonp_si
		
		coxHR_si[k]=HR_cox_si
		cox_p_si[k]=pvalue_cox_si
	}
	
	return(list(r_Z=r_Z, X_si=X_si, U_si=U_si, C_si=C_si, CEN_si=CEN_si, X0_si=X0_si, CEN0_si=CEN0_si, chi_si=chi_si, p_si=p_si, WCchi_si=WCchi_si, WCp_si=WCp_si, coxHR_si=coxHR_si, cox_p_si=cox_p_si))
	
}

