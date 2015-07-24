# 
# Author: Linh Tran
# Date: May 12, 2015
###############################################################################


plotm1 = function(surv, gp) {
	
	plot(NULL, xlim=c(0,20), ylim=c(0,1), xlab="Time", ylab="Survival", main="", yaxt="n", xaxt="n")
	rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="gray90")
	abline(v=seq(0,20,4), col="white", lwd=1.5)
	abline(v=seq(0,20,4/2), col="white", lwd=.75)
	abline(h=seq(0,1,.2), col="white", lwd=1.5)
	abline(h=seq(0,1,.1), col="white", lwd=.75)
	axis(1, seq(0,20,4), labels=TRUE, las=1, tcl=-.25)
	axis(1, seq(0,20,2), labels=FALSE, las=1, tcl=-.25)
	axis(2, seq(0,1,.2), labels=TRUE, las=1)
	axis(2, seq(0,1,.1), labels=FALSE, las=1, tcl=-.25)
	lines(surv ~ c(0:20), col=3, lwd=1.5, type="s")
	lines(gp[1:21] ~ c(0:20), col=4, lwd=1.5, type="s")	
	legend(0, .2, c("Hodgkin's Lymphoma", "US General Population"), fill=c(3,4), bg="white", cex=.8)
	
}



plotm2 = function(GP, hod) {
	
	plot(NULL, xlim=c(0,20), ylim=c(0,.06), xlab="Time", ylab="Hazard", main="", yaxt="n", xaxt="n")
	rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="gray90")
	abline(v=seq(0,20,4), col="white", lwd=1.5)
	abline(v=seq(0,20,4/2), col="white", lwd=.75)
	abline(h=seq(0,.06,.01), col="white", lwd=1.5)
	abline(h=seq(0,.06,.005), col="white", lwd=.75)
	axis(1, seq(0,20,4), labels=TRUE, las=1, tcl=-.25)
	axis(1, seq(0,20,2), labels=FALSE, las=1, tcl=-.25)
	axis(2, seq(0,.06,.01), labels=TRUE, las=1)
	axis(2, seq(0,.06,.005), labels=FALSE, las=1, tcl=-.25)
	for(i in 0:20) {
		lines(rep(GP[i+1],2) ~ c(i,i+1), col=4, lwd=1.5)
	}
	points(GP[1:21] ~ c(1:21), col=4)
	points(GP[1:21] ~ c(0:20), pch=19, col=4)
	for(i in 0:20) {
		lines(rep(hod[i+1],2) ~ c(i,i+1), col=3, lwd=1.5)
	}
	points(hod ~ c(1:21), col=3)
	points(hod ~ c(0:20), pch=19, col=3)
	legend(10, .058, c("Hodgkin's Lymphoma", "US General Population"), fill=c(3,4), bg="white", cex=.8)
	
}



plotm3 = function(hod) {
	
	p1 = ggplot(subset(hod, male==0 & over45==0), aes(x=T_months/12, y=SURVIVAL, colour = factor(stage))) +
			geom_line(size=1.5) + 
			scale_x_continuous(limits = c(0, 20)) +
			scale_y_continuous(limits = c(0, 1)) +
			labs(colour = "", title="Females (Under 45)", x="Time (years)", y="Survival") + 		
			scale_color_discrete(name="Ann Arbor Stage", labels=c("Stage 1/2", "Stage 3", "Stage 4")) +
			theme(legend.justification=c(0,0), legend.position=c(0.05,.05)) 
	p2 = ggplot(subset(hod, male==0 & over45==1), aes(x=T_months/12, y=SURVIVAL, colour = factor(stage))) +
			geom_line(size=1.5) + 
			scale_x_continuous(limits = c(0, 20)) +
			scale_y_continuous(limits = c(0, 1)) +
			labs(colour = "", title="Females (Over 45)", x="Time (years)", y="Survival") + 		
			scale_color_discrete(name="Ann Arbor Stage", labels=c("Stage 1/2", "Stage 3", "Stage 4")) +
			theme(legend.justification=c(0,0), legend.position=c(0.05,.05)) 
	p3 = ggplot(subset(hod, male==1 & over45==0), aes(x=T_months/12, y=SURVIVAL, colour = factor(stage))) +
			geom_line(size=1.5) + 
			scale_x_continuous(limits = c(0, 20)) +
			scale_y_continuous(limits = c(0, 1)) +
			labs(colour = "", title="Males (Under 45)", x="Time (years)", y="Survival") + 		
			scale_color_discrete(name="Ann Arbor Stage", labels=c("Stage 1/2", "Stage 3", "Stage 4")) +
			theme(legend.justification=c(0,0), legend.position=c(0.05,.05)) 
	p4 = ggplot(subset(hod, male==1 & over45==1), aes(x=T_months/12, y=SURVIVAL, colour = factor(stage))) +
			geom_line(size=1.5) + 
			scale_x_continuous(limits = c(0, 20)) +
			scale_y_continuous(limits = c(0, 1)) +
			labs(colour = "", title="Males (Over 45)", x="Time (years)", y="Survival") + 		
			scale_color_discrete(name="Ann Arbor Stage", labels=c("Stage 1/2", "Stage 3", "Stage 4")) +
			theme(legend.justification=c(0,0), legend.position=c(0.05,.05)) 
	multiplot(p1, p3, p2, p4, cols=2)
	
}



plotm4 = function(hod=hodgkins.f) {
	
	pt1 = subset(hod, male==1 & over45==0 & stage==1)$SURVIVAL
	pt2 = subset(hod, male==0 & over45==1 & stage==3)$SURVIVAL
	pt3 = subset(hod, male==1 & over45==1 & stage==2)$SURVIVAL
	gggrp = factor(c(rep("Pt#1", length(pt1)), rep("Pt#2", length(pt2)), rep("Pt#3", length(pt3))))
	ggdata = data.frame(cbind(surv = c(pt1, pt2, pt3), gggrp))
	ggdata$time = rep(c(0:20), 3)
	ggplot(ggdata, aes(x=time, y=surv, colour = factor(gggrp))) +
			geom_line(size=1.5) + 
			labs(colour = "", title="Example", x="Time (years)", y="Survival") + 
			scale_color_discrete(name="Patient drawn", labels=c("Patient #1", "Patient #2", "Patient #3")) +
			theme(legend.justification=c(0,0), legend.position=c(0.05,.05)) 
	
}



plot1 = function(n) {	
	continuous.approach = rexp(n=n, rate=mx)
	discrete.approach = generateData(n=n, lambda=mx, time=100, p=0.5, alpha=0)$data
	## PLOT ##
	ggdata = data.frame(x = c(continuous.approach, discrete.approach$T), g = gl(2,n))
	ggdata$grp = ifelse(ggdata$g==1, "Continuous", ifelse(ggdata$g==2, "Discrete", ""))
	ggplot(ggdata, aes(x, colour = grp)) +
			scale_x_continuous(limits = c(0, 100)) +
			stat_ecdf(size=1.5) + 
			labs(colour = "Approach", title=paste("Simulation", " (n=", n, ")", sep=""), x="Years", y=expression(paste(hat("F")["n"]))) +
			theme(legend.justification=c(1,0), legend.position=c(1,0))
}



plot2 = function(n, mx, US.mx) {
	
	# Exponential vs NP approach
	exp.approach = generateData(n, lambda=mx, time=100, p=0.5, alpha=0)$data
	np.approach = generateData(n, lambda=US.mx, time=100, p=0.5, alpha=0)$data
	np.fit = survfit(Surv(T, delta==1)~1, data=np.approach)
	exp.fit = survfit(Surv(T, delta==1)~1, data=exp.approach)
	## PLOT ##
	npData = data.frame(time=np.fit$time, surv=np.fit$surv, grp="Non-parametric")
	expData = data.frame(time=exp.fit$time, surv=exp.fit$surv, grp="Exponential")
	ggdata = rbind(npData, expData)
	ggplot(ggdata, aes(x=time, y=surv, colour = grp)) +
			scale_x_continuous(limits = c(0, 100)) +
			geom_line(size=1.5) + 
			labs(colour = "Approach", title=paste("Simulation", " (n=", n, ")", sep=""), x="Years", y=expression(paste(hat("S")["n"]))) +
			theme(legend.justification=c(0,0), legend.position=c(0,0))
	
}



plot3 = function(n) {
	
	np.approach = generateData(n, lambda=US.GP.mx$m.x., time=100, p=0.5, alpha=0)$data
	np.fit = survfit(Surv(T, delta==1)~1, data=np.approach)
	S.t = US.GP.mx$l.x./100000
	## PLOT ##
	plot(NULL, xlim=c(0,100), ylim=c(0,1), xlab="Years", ylab="Survival (%)", xaxt="n", yaxt="n")
	rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="gray90")
	abline(v=seq(0,100,25), col="white", lwd=1.5)
	abline(v=seq(0,100,25/2), col="white", lwd=.75)
	abline(h=seq(0,1,.25), col="white", lwd=1.5)
	abline(h=seq(0,1,.25/2), col="white", lwd=.75)
	axis(1, seq(0,100,25), labels=TRUE, las=1)
	axis(2, seq(0,1,.25), labels=TRUE, las=1)
	lines(S.t ~ c(0:100), col=2, lwd=1.5)
	lines(np.fit, lwd=2)
	legend(0, 0.2, c(paste("Empirical (n=", n, ")", sep=""), "Truth"), col=c(1,2), lwd=c(2,1.5), bg="white")
	
}



plot4 = function(n, mx, US.mx) {

	psi_0 = 0.25
	tx.exp.data = generateData(n, lambda=mx, time=100, p=0.5, alpha=psi_0)$data
	tx.exp.fit = survfit(Surv(T, delta==1)~A.1, data=tx.exp.data)
	tx.np.data = generateData(n, lambda=US.mx, time=100, p=0.5, alpha=psi_0)$data
	tx.np.fit = survfit(Surv(T, delta==1)~A.1, data=tx.np.data)
	## PLOT ##
	ggExpData = data.frame(time=tx.exp.fit$time, surv=tx.exp.fit$surv, grp=rep(names(tx.exp.fit$strata), times=tx.exp.fit$strata))
	p1 = ggplot(ggExpData, aes(x=time, y=surv, colour = grp)) +
			scale_x_continuous(limits = c(0, 100)) +
			geom_line(size=1.5) + 
			labs(colour = "TX group", title=expression(paste("Exponential approach", " (", alpha, "=0.25; n=5000)")), x="Years", y=expression(paste(hat("S")["n"]))) +
			theme(legend.justification=c(0,0), legend.position=c(0,0))
	ggNpData = data.frame(time=tx.np.fit$time, surv=tx.np.fit$surv, grp=rep(names(tx.np.fit$strata), times=tx.np.fit$strata))
	p2 = ggplot(ggNpData, aes(x=time, y=surv, colour = grp)) +
			scale_x_continuous(limits = c(0, 100)) +
			geom_line(size=1.5) + 
			labs(colour = "TX group", title=expression(paste("Non-parametric approach", " (", alpha, "=0.25; n=5000)", sep="")), x="Years", y=expression(paste(hat("S")["n"]))) +
			theme(legend.justification=c(0,0), legend.position=c(0,0))
	multiplot(p1, p2, cols=2)
	
}



plot5 = function(n=500, nsim=1000, alpha, psi) {
	
	estimates = estimates2 = estimates3 = matrix(nrow=nsim, ncol=4, dimnames=list(NULL, c("logrank", "wilcoxon", "cox.wald", "cox.hr")))
	for(sim in 1:nsim) {
		cat("sim", sim, "\n")
		## EXPONENTIAL CONTINUOUS ##
		TX = rbinom(n, 1, .5)
		T = ifelse(TX==0, rexp(n, rate=log(2)/10), rexp(n, rate=log(2)/10)*exp(alpha))
		fit = rpsftm(T=T, A=TX, R=TX, Y=rep(1,n), id=seq(1,n), C=rep(100000,n), psi=psi)
		estimates[sim,] = fit$estimates
		## EXPONENTIAL DISCRETE ##
		genData = generateData(n, lambda=log(2)/10, time=120, p=0.5, alpha=alpha)$data
		fit = rpsftm(T=genData$T, A=genData$A.1, R=genData$A.1, C=rep(100000,n), Y=rep(1,n), id=genData$id, psi=psi)
		estimates2[sim,] = fit$estimates
		## NONPARAMETRIC DISCRETE ##
		#nb. Increased hazards slightly to be more comparable to the scenarios above
		genData = generateData(n, lambda=US.GP.mx$m.x.+.01, time=120, p=0.5, alpha=alpha)$data
		fit = rpsftm(T=genData$T, A=genData$A.1, R=genData$A.1, C=rep(100000,n), Y=rep(1,n), id=genData$id, psi=psi)
		estimates3[sim,] = fit$estimates
	}
	
	## PLOT ##
	ggest = rbind(estimates[,"logrank",drop=F], estimates[,"wilcoxon", drop=F], estimates[,"cox.wald", drop=F], estimates[,"cox.hr", drop=F])
	gggrp = c(rep("Log-rank", nrow(estimates)), rep("Wilcoxon", nrow(estimates)), rep("Cox-wald", nrow(estimates)), rep("Cox-HR", nrow(estimates))) 
	ggdata = data.frame(estimates=as.numeric(ggest), grp=gggrp)
	p1 = ggplot(ggdata, aes(x=estimates, colour = grp)) +
			geom_density(size=1.5) +
			geom_vline(xintercept=-alpha, colour="red", size=1.5) +
			scale_x_continuous(limits = c(min(psi), max(psi))) +
			labs(colour = "", title=expression(paste("Continuous Exp(",lambda[0],"=log(2)/10)")), x=expression(widehat(psi)), y="Density") +
			theme(legend.justification=c(0,0), legend.position="top")
	ggest = rbind(estimates2[,"logrank",drop=F], estimates2[,"wilcoxon", drop=F], estimates2[,"cox.wald", drop=F], estimates2[,"cox.hr", drop=F])
	gggrp = c(rep("Log-rank", nrow(estimates2)), rep("Wilcoxon", nrow(estimates2)), rep("Cox-wald", nrow(estimates2)), rep("Cox-HR", nrow(estimates2))) 
	ggdata = data.frame(estimates=as.numeric(ggest), grp=gggrp)
	p2 = ggplot(ggdata, aes(x=estimates, colour = grp)) +
			geom_density(size=1.5) +
			geom_vline(xintercept=-alpha, colour="red", size=1.5) +
			scale_x_continuous(limits = c(min(psi), max(psi))) +
			labs(colour = "", title=expression(paste("Discrete Exp(",lambda[0],"(t)=log(2)/10)")), x=expression(widehat(psi)), y="Density") +
			theme(legend.justification=c(0,0), legend.position="top")
	ggest = rbind(estimates3[,"logrank",drop=F], estimates3[,"wilcoxon", drop=F], estimates3[,"cox.wald", drop=F], estimates3[,"cox.hr", drop=F])
	gggrp = c(rep("Log-rank", nrow(estimates3)), rep("Wilcoxon", nrow(estimates3)), rep("Cox-wald", nrow(estimates3)), rep("Cox-HR", nrow(estimates3))) 
	ggdata = data.frame(estimates=as.numeric(ggest), grp=gggrp)
	p3 = ggplot(ggdata, aes(x=estimates, colour = grp)) +
			geom_density(size=1.5) +
			geom_vline(xintercept=-alpha, colour="red", size=1.5) +
			scale_x_continuous(limits = c(min(psi), max(psi))) +
			labs(colour = "", title=expression(paste("Discrete Exp(",lambda["0,np"],"(t))")), x=expression(widehat(psi)), y="Density") +
			theme(legend.justification=c(0,0), legend.position="top")
	multiplot(p1, p2, p3, cols=3)

}



plot6 = function(n=500, nsim=1000, alpha, psi) {
	
	estimates2 = estimates3 = matrix(nrow=nsim, ncol=4, dimnames=list(NULL, c("logrank", "wilcoxon", "cox.wald", "cox.hr")))
	for(sim in 1:nsim) {
		cat("sim", sim, "\n")
		## EXPONENTIAL DISCRETE ##
		genData = generateData(n, lambda=log(2)/10, time=120, p=0.5, alpha=alpha, c=.01)$data	
		C = ifelse(genData$delta==1, 100, genData$T)
		fit = rpsftm(T=genData$T, A=genData$A.1, R=genData$A.1, C=C, Y=genData$delta, id=genData$id, psi=psi)
		estimates2[sim,] = fit$estimates
		## NONPARAMETRIC DISCRETE ##
		#nb. Increased hazards slightly to be more comparable to the scenarios above
		genData = generateData(n, lambda=US.GP.mx$m.x.+.01, time=120, p=0.5, alpha=alpha, c=.01)$data
		C = ifelse(genData$delta==1, 100, genData$T)
		fit = rpsftm(T=genData$T, A=genData$A.1, R=genData$A.1, C=C, Y=genData$delta, id=genData$id, psi=psi)
		estimates3[sim,] = fit$estimates
	}
	## PLOT ##
	ggest = rbind(estimates2[,"logrank",drop=F], estimates2[,"wilcoxon", drop=F], estimates2[,"cox.wald", drop=F], estimates2[,"cox.hr", drop=F])
	gggrp = c(rep("Log-rank", nrow(estimates2)), rep("Wilcoxon", nrow(estimates2)), rep("Cox-wald", nrow(estimates2)), rep("Cox-HR", nrow(estimates2)))
	ggdata = data.frame(estimates=as.numeric(ggest), grp=gggrp)
	p2 = ggplot(ggdata, aes(x=estimates, colour = grp)) +
			geom_density(size=1.5) +
			geom_vline(xintercept=-alpha, colour="red", size=1.5) +
			scale_x_continuous(limits = c(min(psi), max(psi))) +
			labs(colour = "", title=expression(paste("Discrete Exp(",lambda[0],"(t)=log(2)/10)")), x=expression(widehat(psi)), y="Density") +
			theme(legend.justification=c(0,0), legend.position="top")
	ggest = rbind(estimates3[,"logrank",drop=F], estimates3[,"wilcoxon", drop=F], estimates3[,"cox.wald", drop=F], estimates3[,"cox.hr", drop=F])
	gggrp = c(rep("Log-rank", nrow(estimates3)), rep("Wilcoxon", nrow(estimates3)), rep("Cox-wald", nrow(estimates3)), rep("Cox-HR", nrow(estimates3))) 
	ggdata = data.frame(estimates=as.numeric(ggest), grp=gggrp)
	p3 = ggplot(ggdata, aes(x=estimates, colour = grp)) +
			geom_density(size=1.5) +
			geom_vline(xintercept=-alpha, colour="red", size=1.5) +
			scale_x_continuous(limits = c(min(psi), max(psi))) +
			labs(colour = "", title=expression(paste("Discrete Exp(",lambda["0,np"],"(t))")), x=expression(widehat(psi)), y="Density") +
			theme(legend.justification=c(0,0), legend.position="top")
	multiplot(p2, p3, cols=2)
	
}



plot7 = function(alpha, US.mx) {
	
	genData = generateData(n=500, lambda=US.mx, time=120, p=0.5, alpha=alpha, c=.01)$data
	dates = rep(seq(from=as.Date("2011-01-01"), to=as.Date("2011-01-01")+500/10*30.5, by="month"), 10)
	genData$date = dates[1:nrow(genData)]
	plot(NULL, xlim=c(0,100), ylim=c(0,50), xlab="Date", ylab="", xaxt="n", yaxt="n")
	rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="gray90")
	abline(v=seq(0,100,20), col="white", lwd=1.5)
	abline(v=seq(0,100,10), col="white", lwd=.75)
	axis(1, seq(0,100,20), labels=c("Jan2011", "Sep2012", "May2014", "Jan2016", "Sep2017", "May2019"), las=1)
	axis(1, seq(0,100,10), labels=FALSE, las=1, tcl=-.25)
	for(i in 1:25) {
		date = floor(as.numeric(dates[i] - as.Date("2011-01-01"))/30)
		## CONTROL ##
		tmp = subset(genData, A.1==0 & date==dates[i])[1,]
		lines(rep(50-(i+1)+.5,2) ~ c(date, date+tmp$T), lwd=2, col=2)
		pch = ifelse(tmp$delta==1, 4, 1)
		points(y=50-(i+1)+.5, x=date+tmp$T, lwd=1.5, col=2, pch=pch)
		## TREATMENT ##
		tmp = subset(genData, A.1==1 & date==dates[i])[1,]
		lines(rep(25-(i+1)+.5,2) ~ c(date, date+tmp$T), lwd=2, col=4)
		pch = ifelse(tmp$delta==1, 4, 1)
		points(y=25-(i+1)+.5, x=date+tmp$T, lwd=1.5, col=4, pch=pch)
	}
	abline(v=60, lty=2, lwd=2)
	text(x=60, y=55, labels="Analysis\ndate", xpd=NA)	
	legend(80,50, c("Failure", "Censor"), pch=c(4,1), bg="white", cex=1.25)
}



plot8 = function(n=500, eventsDesired=200, nsim=1000, alpha, psi, US.mx) {
	estimates = matrix(nrow=nsim, ncol=4, dimnames=list(NULL, c("logrank", "wilcoxon", "cox.wald", "cox.hr")))
	for(sim in 1:nsim) {
		cat("sim", sim, "\n")
		## GENERATE DATA ##
		genData = suppressWarnings(generateData(n, lambda=US.mx, time=120, p=0.5, alpha=alpha, c=.01)$data)
		dates = rep(seq(from=as.Date("2011-01-01"), to=as.Date("2011-01-01")+500/10*30.5, by="month"), 10)
		## SUBSET OUT ANALYSIS DATA ##
		genData$dateStart = dates[1:nrow(genData)]
		genData$dateEnd = genData$dateStart + (365.25/12)*genData$T
		dateAnalyzed = getAnalysisDate(genData$dateEnd, genData$delta, eventsDesired)
		analysisData = subset(genData, dateStart < dateAnalyzed)
		## UPDATE CENSORING ##
		analysisData$delta = ifelse(analysisData$dateEnd<dateAnalyzed, analysisData$delta, 0)
		analysisData$T = ifelse(analysisData$dateEnd<dateAnalyzed, analysisData$T, round(as.numeric(dateAnalyzed-genData$dateStart)/(365.25/12)))
		analysisData$C = ifelse(analysisData$delta==1, floor(as.numeric(dateAnalyzed-genData$dateStart)/(365.25/12)), analysisData$T)
		## GET ESTIMATES ##
		fit = rpsftm(T=analysisData$T, A=analysisData$A.1, R=analysisData$A.1, C=analysisData$C, Y=analysisData$delta, id=analysisData$id, psi=psi)
		estimates[sim,] = fit$estimates
	}
	## PLOT ##
	ggest = rbind(estimates[,"logrank",drop=F], estimates[,"wilcoxon", drop=F], estimates[,"cox.wald", drop=F], estimates[,"cox.hr", drop=F])
	gggrp = c(rep("Log-rank", nrow(estimates)), rep("Wilcoxon", nrow(estimates)), rep("Cox-wald", nrow(estimates)), rep("Cox-HR", nrow(estimates)))
	ggdata = data.frame(estimates=as.numeric(ggest), grp=gggrp)
	ggplot(ggdata, aes(x=estimates, colour = grp)) +
			geom_density(size=1.5) +
			geom_vline(xintercept=-alpha, colour="red", size=1.5) +
			scale_x_continuous(limits = c(min(psi), max(psi))) +
			labs(colour = "", title=expression(paste("Discrete Exp(",lambda["0,np"],"(t))")), x=expression(widehat(psi)), y="Density") +
			theme(legend.justification=c(0,0), legend.position="top")
	
}



plot9 = function(n) {
	
	O = NULL
	for(j in 1:n) {
		male = rbinom(1, 1,.5)
		age = rpois(1, lambda=12) + 18
		if(male==1) {
			lambda.gender = US.M
		} else {
			lambda.gender = US.F
		}
		lambda = lambda.gender[(age+1):length(lambda.gender)]
		genData = suppressWarnings(generateData(1, lambda=lambda, time=100, p=0.5, alpha=0.2, c=.01)$data)
		genData$male = male; genData$age = age
		O = rbind(O, genData)
	}
	p1 = ggplot(subset(O, A.1==0), aes(x=age, fill=factor(male))) + 
			geom_histogram(binwidth=1) +
			scale_x_continuous(limits = c(20, 40)) +
			labs(fill="Male", title=paste("Control", " (n=", sum(O$A.1==0), ")", sep=""), x="Age", y="Count") +
			theme(legend.justification=c(0.5,0), legend.position=c(.9,.75))
	p2 = ggplot(subset(O, A.1==1), aes(x=age, fill=factor(male))) + 
			geom_histogram(binwidth=1) +
			scale_x_continuous(limits = c(20, 40)) +
			labs(fill="Male", title=paste("Treatment", " (n=", sum(O$A.1==1), ")", sep=""), x="Age", y="Count") +
			theme(legend.justification=c(0.5,0), legend.position=c(.9,.75))
	multiplot(p1, p2, cols=2)
	
}



plot10 = function(n=500, eventsDesired=200, nsim=1000, alpha, psi) {
	
	estimates = matrix(nrow=nsim, ncol=4, dimnames=list(NULL, c("logrank", "wilcoxon", "cox.wald", "cox.hr")))
	for(sim in 1:nsim) {
		cat("sim", sim, "\n")
		## RANDOMLY DRAWS FROM GP AND OBSERVES SURVIVAL ##
		O = do.call("rbind", mclapply(1:n, function(j) {
							male = rbinom(1, 1,.5)
							age = rpois(1, lambda=12) + 18
							if(male==1) {
								lambda.gender = US.M
							} else {
								lambda.gender = US.F
							}
							lambda = lambda.gender[(age+1):length(lambda.gender)]
							genData = suppressWarnings(generateData(1, lambda=lambda, time=100, p=0.5, alpha=alpha, c=.01)$data)
							genData$id = j; genData$male = male; genData$age = age
							return(genData)
						}))
		## SUBSET OUT ANALYSIS DATA ##
		dates = rep(seq(from=as.Date("2011-01-01"), to=as.Date("2011-01-01")+n/10*30.5, by="month"), 10)
		O$dateStart = dates[1:nrow(O)]
		O$dateEnd = O$dateStart + (365.25/12)*O$T
		dateAnalyzed = getAnalysisDate(O$dateEnd, O$delta, eventsDesired)
		analysisData = subset(O, dateStart < dateAnalyzed)
		## UPDATE CENSORING ##
		analysisData$delta = ifelse(analysisData$dateEnd<dateAnalyzed, analysisData$delta, 0)
		analysisData$T = ifelse(analysisData$dateEnd<dateAnalyzed, analysisData$T, round(as.numeric(dateAnalyzed-analysisData$dateStart)/(365.25/12)))
		analysisData$C = ifelse(analysisData$delta==1, floor(as.numeric(dateAnalyzed-analysisData$dateStart)/(365.25/12)), analysisData$T)
		## GET ESTIMATES ##
		fit = rpsftm(T=analysisData$T, A=analysisData$A.1, R=analysisData$A.1, C=analysisData$C, Y=analysisData$delta, id=analysisData$id, psi=psi)
		estimates[sim,] = fit$estimates
	}
	## PLOT ##
	ggest = rbind(estimates[,"logrank",drop=F], estimates[,"wilcoxon", drop=F], estimates[,"cox.wald", drop=F], estimates[,"cox.hr", drop=F])
	gggrp = c(rep("Log-rank", nrow(estimates)), rep("Wilcoxon", nrow(estimates)), rep("Cox-wald", nrow(estimates)), rep("Cox-HR", nrow(estimates)))
	ggdata = data.frame(estimates=as.numeric(ggest), grp=gggrp)
	ggplot(ggdata, aes(x=estimates, colour = grp)) +
			geom_density(size=1.5) +
			geom_vline(xintercept=-alpha, colour="red", size=1.5) +
			scale_x_continuous(limits = c(min(psi), max(psi))) +
			labs(colour = "", title="", x=expression(widehat(psi)), y="Density") +
			theme(legend.justification=c(0,0), legend.position="top")
	
}



plot11 = function(n=500, eventsDesired=200, nsim=1000, alpha, psi, US.mx) {

	estMine = estConnie = matrix(nrow=nsim, ncol=4, dimnames=list(NULL, c("logrank", "wilcoxon", "cox.wald", "cox.hr")))
	for(sim in 1:nsim) {
		cat("Sim", sim, "\n")
		## GENERATE  DATA ##
		genData = suppressWarnings(generateData(n, lambda=US.mx, time=120, p=0.5, alpha=alpha, c=.01)$data)
		dates = rep(seq(from=as.Date("2011-01-01"), to=as.Date("2011-01-01")+500/10*30.5, by="month"), 10)
		## SUBSET OUT ANALYSIS DATA ##
		genData$dateStart = dates[1:nrow(genData)]
		genData$dateEnd = genData$dateStart + (365.25/12)*genData$T
		dateAnalyzed = getAnalysisDate(genData$dateEnd, genData$delta, eventsDesired)
		analysisData = subset(genData, dateStart < dateAnalyzed)
		## UPDATE CENSORING ##
		analysisData$delta = ifelse(analysisData$dateEnd<dateAnalyzed, analysisData$delta, 0)
		analysisData$T = ifelse(analysisData$dateEnd<dateAnalyzed, analysisData$T, round(as.numeric(dateAnalyzed-genData$dateStart)/(365.25/12)))
		analysisData$C = ifelse(analysisData$delta==1, floor(as.numeric(dateAnalyzed-genData$dateStart)/(365.25/12)), analysisData$T)
		## MY ESTIMATES ##
		fit = rpsftm(T=analysisData$T, A=analysisData$A.1, R=analysisData$A.1, C=analysisData$C, Y=analysisData$delta, id=analysisData$id, psi=psi)
		estMine[sim,] = fit$estimates
		## CONNIE's ESTIMATES ##
		git = crpsftm(cbind(Z=analysisData$A.1, futime=floor(as.numeric(dateAnalyzed-genData$dateStart)/(365.25/12)), TALT=analysisData$T, T=analysisData$T, C=analysisData$delta, ALT=rep(0,nrow(analysisData)), D=analysisData$T, DIS=analysisData$delta, strata=rep(1,nrow(analysisData))), si=psi)
		estConnie[sim,"logrank"] = psi[which.max(git$p_si)]
		estConnie[sim,"wilcoxon"] = psi[which.max(git$WCp_si)]
		estConnie[sim,"cox.wald"] = psi[which.max(git$cox_p_si)]
		estConnie[sim,"cox.hr"] = psi[which.min(log(git$coxHR_si)^2)]	
	}
	ggest = rbind(estMine[,"logrank",drop=F], estMine[,"wilcoxon", drop=F], estMine[,"cox.wald", drop=F], estMine[,"cox.hr", drop=F])
	gggrp = c(rep("Log-rank", nrow(estMine)), rep("Wilcoxon", nrow(estMine)), rep("Cox-wald", nrow(estMine)), rep("Cox-HR", nrow(estMine)))
	ggdata = data.frame(estimates=as.numeric(ggest), grp=gggrp)
	p1 = ggplot(ggdata, aes(x=estimates, colour = grp)) +
			geom_density(size=1.5) +
			geom_vline(xintercept=-alpha, colour="red", size=1.5) +
			scale_x_continuous(limits = c(min(psi), max(psi))) +
			labs(colour = "", title="Linh's Approach", x=expression(widehat(psi)), y="Density") +
			theme(legend.justification=c(0,0), legend.position="top")
	ggest = rbind(estConnie[,"logrank",drop=F], estConnie[,"wilcoxon", drop=F], estConnie[,"cox.wald", drop=F], estConnie[,"cox.hr", drop=F])
	gggrp = c(rep("Log-rank", nrow(estConnie)), rep("Wilcoxon", nrow(estConnie)), rep("Cox-wald", nrow(estConnie)), rep("Cox-HR", nrow(estConnie)))
	ggdata = data.frame(estimates=as.numeric(ggest), grp=gggrp)
	p2 = ggplot(ggdata, aes(x=estimates, colour = grp)) +
			geom_density(size=1.5) +
			geom_vline(xintercept=-alpha, colour="red", size=1.5) +
			scale_x_continuous(limits = c(min(psi), max(psi))) +
			labs(colour = "", title="Connie's Approach", x=expression(widehat(psi)), y="Density") +
			theme(legend.justification=c(0,0), legend.position="top")
	multiplot(p1, p2, cols=2)
	
}



plot12 = function(fit) {
	
	plot(NULL, xlim=c(0,100), ylim=c(0,1), xlab="Time", ylab="Survival", yaxt="n")
	rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="gray90")
	abline(v=seq(0,100,20), col="white", lwd=1.5)
	abline(v=seq(0,100,10), col="white", lwd=.75)
	abline(h=seq(0,1,.20), col="white", lwd=1.5)
	abline(h=seq(0,1,.10), col="white", lwd=.75)
	axis(1, seq(0,100,20), labels=TRUE, las=1)
	axis(1, seq(0,100,10), labels=FALSE, las=1, tcl=-.25)
	axis(2, seq(0,1,.2), labels=TRUE, las=1)
	axis(2, seq(0,1,.1), labels=FALSE, las=1, tcl=-.25)
	abline(h=.5, lwd=1.5, col="red", lty=2)
	lines(fit, lwd=2)
	abline(v=25, lwd=2, col=1)
	abline(v=42, lwd=2, col="darkgray", lty=2)
	abline(v=46, lwd=2, col="darkgray", lty=2)
	mtext("Crossover", 3, at=25)
	mtext("42", 3, at=42-1)
	mtext("46", 3, at=46+1)

}



plot13 = function(fit1, fit2) {
	
	plot(NULL, xlim=c(0,100), ylim=c(0,1), xlab="Time", ylab="Survival", main="One-way crossover (Control-->Treatment)", yaxt="n")
	rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="gray90")
	abline(v=seq(0,100,20), col="white", lwd=1.5)
	abline(v=seq(0,100,10), col="white", lwd=.75)
	abline(h=seq(0,1,.20), col="white", lwd=1.5)
	abline(h=seq(0,1,.10), col="white", lwd=.75)
	axis(1, seq(0,100,20), labels=TRUE, las=1)
	axis(1, seq(0,100,10), labels=FALSE, las=1, tcl=-.25)
	axis(2, seq(0,1,.2), labels=TRUE, las=1)
	axis(2, seq(0,1,.1), labels=FALSE, las=1, tcl=-.25)
	abline(h=.5, lwd=1.5)
	lines(fit1, col=c(2), lwd=2, mark.time=FALSE, conf.int=FALSE)
	lines(fit2, col=c(2,4), lwd=2, lty=c(2,1))
	legend(45,1, c("Active Treatment", "Control (no crossover)", "Control (crossovers)"), col=c(4,2,2), lty=c(1,1,2), lwd=2, bg="white", cex=.8)
	
}


plot13b = function(fit1, fit2, fit3) {
	
	plot(NULL, xlim=c(0,100), ylim=c(0,1), xlab="Time", ylab="Survival", main="One-way crossover (Control-->Treatment)", yaxt="n")
	rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="gray90")
	abline(v=seq(0,100,20), col="white", lwd=1.5)
	abline(v=seq(0,100,10), col="white", lwd=.75)
	abline(h=seq(0,1,.20), col="white", lwd=1.5)
	abline(h=seq(0,1,.10), col="white", lwd=.75)
	axis(1, seq(0,100,20), labels=TRUE, las=1)
	axis(1, seq(0,100,10), labels=FALSE, las=1, tcl=-.25)
	axis(2, seq(0,1,.2), labels=TRUE, las=1)
	axis(2, seq(0,1,.1), labels=FALSE, las=1, tcl=-.25)
	abline(h=.5, lwd=1.5)
	lines(fit1, col=c(2), lwd=2, mark.time=FALSE, conf.int=FALSE)
	lines(fit2, col=c(0,4), lwd=2, lty=c(0,1))
	lines(fit3, col=c(2), lwd=2, lty=c(2), mark.time=FALSE, conf.int=FALSE)
	legend(45,1, c("Active Treatment", "Control (no crossover)", "Control (crossovers)"), col=c(4,2,2), lty=c(1,1,2), lwd=2, bg="white", cex=.8)
	
}


plot13c = function(fit1, fit2=NULL, both=TRUE) {
	
	plot(NULL, xlim=c(0,100), ylim=c(0,1), xlab="Time", ylab="Survival", main="No crossovers", yaxt="n")
	rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="gray90")
	abline(v=seq(0,100,20), col="white", lwd=1.5)
	abline(v=seq(0,100,10), col="white", lwd=.75)
	abline(h=seq(0,1,.20), col="white", lwd=1.5)
	abline(h=seq(0,1,.10), col="white", lwd=.75)
	axis(1, seq(0,100,20), labels=TRUE, las=1)
	axis(1, seq(0,100,10), labels=FALSE, las=1, tcl=-.25)
	axis(2, seq(0,1,.2), labels=TRUE, las=1)
	axis(2, seq(0,1,.1), labels=FALSE, las=1, tcl=-.25)
	abline(h=.5, lwd=1.5)
	if(both) {
		lines(fit1, col=c(2), lwd=2, mark.time=FALSE, conf.int=FALSE)
		lines(fit2, col=c(0,4), lwd=2, lty=c(0,1))
		legend(45,1, c("Active Treatment", "Control"), col=c(4,2), lty=c(1,1), lwd=2, bg="white", cex=.8)
	} else {
		lines(fit1, col=c(4), lwd=2, mark.time=FALSE, conf.int=FALSE)
		legend(35,1, c("Counterfactual survival off TX"), col=c(4), lty=c(1), lwd=2, bg="white", cex=.8)		
	}
	
}

plot14 = function(fit1, fit2) {
	
	plot(NULL, xlim=c(0,100), ylim=c(0,1), xlab="Time", ylab="Survival", yaxt="n", main="One-way crossover (Control-->Treatment)")
	rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="gray90")
	abline(v=seq(0,100,20), col="white", lwd=1.5)
	abline(v=seq(0,100,10), col="white", lwd=.75)
	abline(h=seq(0,1,.20), col="white", lwd=1.5)
	abline(h=seq(0,1,.10), col="white", lwd=.75)
	axis(1, seq(0,100,20), labels=TRUE, las=1)
	axis(1, seq(0,100,10), labels=FALSE, las=1, tcl=-.25)
	axis(2, seq(0,1,.2), labels=TRUE, las=1)
	axis(2, seq(0,1,.1), labels=FALSE, las=1, tcl=-.25)
	abline(h=.5, lwd=1.5)
	mtext("Stratifying by whether subject crossed over", 3, at=50, line=0.5)
	lines(fit1, col=c(2), lwd=2, mark.time=FALSE, conf.int=FALSE)
	lines(fit2, lwd=2, lty=c(2,2,1), col=c("gray39",2,4))
	legend(42,1, c("Active Treatment", "Control (no crossover)", expression(paste("Conditioning on A(t)=0 ", symbol("\042"), " t")), "Crossed over at any point"), col=c(4,2,2,"gray39"), lty=c(1,1,2,2), lwd=2, bg="white", cex=.7)
	
}



plot15 = function() {
	
	crossTX = generateData(n=10000, lambda=US.mx, time=120, p=0, alpha=alpha, crossover=.02, long=TRUE)$data
	crossTX$prevA[2:nrow(crossTX)] = crossTX$A[1:(nrow(crossTX)-1)]
	crossTX$prevA[crossTX$time==1] = 0
	crossTX = subset(crossTX, prevA==0)
	crossTX$timeStart = crossTX$time - 1
	fit = survfit(Surv(timeStart, time, A)~1, data=crossTX)
	plot(NULL, xlim=c(0,100), ylim=c(0,1), xlab="Time", ylab="Probability", main="Cumulative probability of crossover", yaxt="n")
	rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="gray90")
	abline(v=seq(0,100,20), col="white", lwd=1.5)
	abline(v=seq(0,100,10), col="white", lwd=.75)
	abline(h=seq(0,1,.20), col="white", lwd=1.5)
	abline(h=seq(0,1,.10), col="white", lwd=.75)
	axis(1, seq(0,100,20), labels=TRUE, las=1)
	axis(1, seq(0,100,10), labels=FALSE, las=1, tcl=-.25)
	axis(2, seq(0,1,.2), labels=TRUE, las=1)
	axis(2, seq(0,1,.1), labels=FALSE, las=1, tcl=-.25)
	lines(fit, fun="event", conf.int=FALSE, mark.time=FALSE, lwd=2)
	
}



plot16 = function(n=500, nsim=1000, alpha=.2, psi=seq(-.6,.2,.005)) {
	
	results1 = results2 = matrix(nrow=nsim, ncol=4, dimnames=list(NULL, c("logrank", "wilcoxon", "cox.wald", "cox.hr")))
	for(sim in 1:nsim) {
		cat("Sim:", sim, "\n")
		genData = generateData(n=n, lambda=mx, time=120, p=0.5, alpha=alpha, crossover=.02, long=TRUE)$data
		genData$t = 1; genData$delta = 1
		genData %<>% subset(time==1, select=c("id", "A")) %>% rename(c(A="R")) %>% merge(genData, by="id")
		fit = rpsftm(T=genData$t, A=genData$A, R=genData$R, C=genData$T*1000, Y=genData$delta, id=genData$id, psi=psi)
		results1[sim,] = fit$estimates
		cat(fit$estimates, "\n")
		genData = generateData(n=n, lambda=US.mx, time=120, p=0.5, alpha=alpha, crossover=.02, long=TRUE)$data
		genData$t = 1; genData$delta = 1
		genData %<>% subset(time==1, select=c("id", "A")) %>% rename(c(A="R")) %>% merge(genData, by="id")
		fit = rpsftm(T=genData$t, A=genData$A, R=genData$R, C=genData$T*1000, Y=genData$delta, id=genData$id, psi=psi)
		results2[sim,] = fit$estimates
		cat(fit$estimates, "\n")
	}
	ggest = rbind(results1[,"logrank",drop=F], results1[,"wilcoxon", drop=F], results1[,"cox.wald", drop=F], results1[,"cox.hr", drop=F])
	gggrp = c(rep("Log-rank", nrow(results1)), rep("Wilcoxon", nrow(results1)), rep("Cox-wald", nrow(results1)), rep("Cox-HR", nrow(results1)))
	ggdata = data.frame(estimates=as.numeric(ggest), grp=gggrp)
	p1 = ggplot(ggdata, aes(x=estimates, colour = grp)) +
			geom_density(size=1.5) +
			geom_vline(xintercept=-alpha, colour="red", size=1.5) +
			scale_x_continuous(limits = c(min(psi), max(psi))) +
			labs(colour = "", title=expression(paste("Exp(", lambda, ")")), x=expression(widehat(psi)), y="Density") +
			theme(legend.justification=c(0,0), legend.position="top")
	ggest = rbind(results2[,"logrank",drop=F], results2[,"wilcoxon", drop=F], results2[,"cox.wald", drop=F], results2[,"cox.hr", drop=F])
	gggrp = c(rep("Log-rank", nrow(results2)), rep("Wilcoxon", nrow(results2)), rep("Cox-wald", nrow(results2)), rep("Cox-HR", nrow(results2)))
	ggdata = data.frame(estimates=as.numeric(ggest), grp=gggrp)
	p2 = ggplot(ggdata, aes(x=estimates, colour = grp)) +
			geom_density(size=1.5) +
			geom_vline(xintercept=-alpha, colour="red", size=1.5) +
			scale_x_continuous(limits = c(min(psi), max(psi))) +
			labs(colour = "", title=expression(paste(lambda["np"])), x=expression(widehat(psi)), y="Density") +
			theme(legend.justification=c(0,0), legend.position="top")
	multiplot(p1, p2, cols=2)
	
}

		

plot17 = function(n=500, nsim=1000, eventsDesired = 200) {
	
	superman = makeCluster(ncores, type=cluster)
	registerDoSNOW(superman)
	cat("Cluster summary\n", getDoParRegistered(), "\n", getDoParName(), "\n", getDoParWorkers(), "\n", getDoParVersion(), "\n") 
	clusterSetupRNG(superman, sed=rep(6,6))
	results1 = foreach(sim=1:nsim, .combine="rbind") %dopar% {
		alpha = 0.2
		psi = seq(-.6,.2,.005)
		require("reshape")
		require("magrittr")
		require("rpsftm")
		require("survival")
		
		## GENERATES DATA ##
		O = suppressWarnings(generateData(n, lambda=US.mx, time=120, p=0.5, alpha=alpha, c=.01, crossover=0.02, long=TRUE)$data)
		O$Y[is.na(O$Y)] = 0
		O %<>% subset(time==1, select=c("id", "A")) %>% rename(c(A="R")) %>% merge(O, by="id")
		## SUBSET OUT ANALYSIS DATA ##
		dates = data.frame(dateStart=rep(seq(from=as.Date("2011-01-01"), to=as.Date("2011-01-01")+n/10*30.5, by="month"), 10))
		dates$id = seq.int(1:nrow(dates))
		O = merge(O, dates, by="id", all.x=TRUE)
		O$dateEnd = O$dateStart + (365.25/12)*O$T
		O$date = O$dateStart + (365.25/12)*(O$time-1)
		dateAnalyzed = with(subset(O, time==1), getAnalysisDate(dateEnd, delta, eventsDesired))
		analysisData = subset(O, date < dateAnalyzed)	
		## UPDATE CENSORING ##
		analysisData$eofu = analysisData$dateEnd>=dateAnalyzed
		analysisData$delta = ifelse(analysisData$eofu, 0, analysisData$delta)
		analysisData$Y = ifelse(analysisData$eofu, 0, analysisData$Y)
		analysisData$T = ifelse(analysisData$eofu, ceiling(as.numeric(dateAnalyzed-analysisData$dateStart)/(365.25/12)), analysisData$T)
		analysisData$C = ifelse(analysisData$delta==1, ceiling(as.numeric(dateAnalyzed-analysisData$dateStart)/(365.25/12)), analysisData$T)	
		## GET ESTIMATES ##
		analysisData$t = 1
		fit = rpsftm(T=analysisData$t, A=analysisData$A, R=analysisData$R, C=analysisData$C, Y=analysisData$delta, id=analysisData$id, psi=psi)
		return(fit$estimates)	
	}
	stopCluster(superman)
	
	superman = makeCluster(ncores, type=cluster)
	registerDoSNOW(superman)
	cat("Cluster summary\n", getDoParRegistered(), "\n", getDoParName(), "\n", getDoParWorkers(), "\n", getDoParVersion(), "\n") 
	clusterSetupRNG(superman, sed=rep(6,6))
	results2 = foreach(sim=1:nsim, .combine="rbind") %dopar% {
		alpha = -0.2
		psi = seq(-.2,.6,.005)
		require("reshape")
		require("magrittr")
		require("rpsftm")
		require("survival")
		
		## GENERATES DATA ##
		O = suppressWarnings(generateData(n, lambda=US.mx, time=120, p=0.5, alpha=alpha, c=.01, crossover=0.02, long=TRUE)$data)
		O$Y[is.na(O$Y)] = 0
		O %<>% subset(time==1, select=c("id", "A")) %>% rename(c(A="R")) %>% merge(O, by="id")
		## SUBSET OUT ANALYSIS DATA ##
		dates = data.frame(dateStart=rep(seq(from=as.Date("2011-01-01"), to=as.Date("2011-01-01")+n/10*30.5, by="month"), 10))
		dates$id = seq.int(1:nrow(dates))
		O = merge(O, dates, by="id", all.x=TRUE)
		O$dateEnd = O$dateStart + (365.25/12)*O$T
		O$date = O$dateStart + (365.25/12)*(O$time-1)
		dateAnalyzed = with(subset(O, time==1), getAnalysisDate(dateEnd, delta, eventsDesired))
		analysisData = subset(O, date < dateAnalyzed)	
		## UPDATE CENSORING ##
		analysisData$eofu = analysisData$dateEnd>=dateAnalyzed
		analysisData$delta = ifelse(analysisData$eofu, 0, analysisData$delta)
		analysisData$Y = ifelse(analysisData$eofu, 0, analysisData$Y)
		analysisData$T = ifelse(analysisData$eofu, ceiling(as.numeric(dateAnalyzed-analysisData$dateStart)/(365.25/12)), analysisData$T)
		analysisData$C = ifelse(analysisData$delta==1, ceiling(as.numeric(dateAnalyzed-analysisData$dateStart)/(365.25/12)), analysisData$T)	
		## GET ESTIMATES ##
		analysisData$t = 1
		fit = rpsftm(T=analysisData$t, A=analysisData$A, R=analysisData$R, C=analysisData$C, Y=analysisData$delta, id=analysisData$id, psi=psi)
		return(fit$estimates)	
	}
	stopCluster(superman)
	
	ggest = rbind(results1[,"Log-rank",drop=F], results1[,"Wilcoxon", drop=F], results1[,"Cox-Wald", drop=F], results1[,"Cox-HR", drop=F])
	gggrp = c(rep("Log-rank", nrow(results1)), rep("Wilcoxon", nrow(results1)), rep("Cox-Wald", nrow(results1)), rep("Cox-HR", nrow(results1)))
	ggdata = data.frame(estimates=as.numeric(ggest), grp=gggrp)
	p1 = ggplot(ggdata, aes(x=estimates, colour = grp)) +
			geom_density(size=1.5) +
			geom_vline(xintercept=-0.2, colour="red", size=1.5) +
			scale_x_continuous(limits = c(-.6, .2)) +
			labs(colour = "", title=expression(paste(alpha, "= 0.2")), x=expression(widehat(psi)), y="Density") +
			theme(legend.justification=c(0,0), legend.position="top")
	ggest = rbind(results2[,"Log-rank",drop=F], results2[,"Wilcoxon", drop=F], results2[,"Cox-Wald", drop=F], results2[,"Cox-HR", drop=F])
	gggrp = c(rep("Log-rank", nrow(results2)), rep("Wilcoxon", nrow(results2)), rep("Cox-wald", nrow(results2)), rep("Cox-HR", nrow(results2)))
	ggdata = data.frame(estimates=as.numeric(ggest), grp=gggrp)
	p2 = ggplot(ggdata, aes(x=estimates, colour = grp)) +
			geom_density(size=1.5) +
			geom_vline(xintercept=0.2, colour="red", size=1.5) +
			scale_x_continuous(limits = c(-.2, .6)) +
			labs(colour = "", title=expression(paste(alpha, "= -0.2")), x=expression(widehat(psi)), y="Density") +
			theme(legend.justification=c(0,0), legend.position="top")
	multiplot(p1, p2, cols=2)
	
}



plot18 = function(n=500, nsim=1000, eventsDesired = 200) {
	
	superman = makeCluster(ncores, type=cluster)
	registerDoSNOW(superman)
	cat("Cluster summary\n", getDoParRegistered(), "\n", getDoParName(), "\n", getDoParWorkers(), "\n", getDoParVersion(), "\n") 
	clusterSetupRNG(superman, sed=rep(6,6))
	results1 = foreach(sim=1:nsim, .combine="rbind") %dopar% {
		alpha = 0.2
		psi = seq(-.6,.2,.005)
		require("reshape")
		require("magrittr")
		require("rpsftm")
		require("survival")
		## RANDOMLY DRAWS FROM GP AND ASSIGNS PT SPECIFIC SURVIVAL ##
		O = do.call("rbind", lapply(1:n, function(j) {
							male = rbinom(1, 1,.5)
							age = rpois(1, lambda=12) + 18
							if(male==1) {
								lambda.gender = US.M
							} else {
								lambda.gender = US.F
							}
							lambda = lambda.gender[(age+1):length(lambda.gender)]
							genData = suppressWarnings(rpsftm::generateData(1, lambda=lambda, time=100, p=0.5, alpha=alpha, c=.01, crossover=0.02, long=TRUE)$data)
							genData$id = j; genData$male = male; genData$age = age
							return(genData)
						}))
		O$Y[is.na(O$Y)] = 0
		O %<>% subset(time==1, select=c("id", "A")) %>% rename(c(A="R")) %>% merge(O, by="id")
		## SUBSET OUT ANALYSIS DATA (ASSUMES MONTHLY TIME INTERVALS) ##
		dates = data.frame(dateStart=rep(seq(from=as.Date("2011-01-01"), to=as.Date("2011-01-01")+n/10*30.5, by="month"), 10))
		dates$id = seq.int(1:nrow(dates))
		O = merge(O, dates, by="id", all.x=TRUE)
		O$dateEnd = O$dateStart + (365.25/12)*O$T
		O$date = O$dateStart + (365.25/12)*(O$time-1)
		dateAnalyzed = with(subset(O, time==1), getAnalysisDate(dateEnd, delta, eventsDesired))
		analysisData = subset(O, date < dateAnalyzed)
		## UPDATE CENSORING ##
		analysisData$eofu = analysisData$dateEnd>=dateAnalyzed
		analysisData$delta = ifelse(analysisData$eofu, 0, analysisData$delta)
		analysisData$Y = ifelse(analysisData$eofu, 0, analysisData$Y)
		analysisData$T = ifelse(analysisData$eofu, ceiling(as.numeric(dateAnalyzed-analysisData$dateStart)/(365.25/12)), analysisData$T)
		analysisData$C = ifelse(analysisData$delta==1, ceiling(as.numeric(dateAnalyzed-analysisData$dateStart)/(365.25/12)), analysisData$T)
		## GET ESTIMATES ##
		analysisData$t = 1
		fit = rpsftm(T=analysisData$t, A=analysisData$A, R=analysisData$R, C=analysisData$C, Y=analysisData$delta, id=analysisData$id, psi=psi)
		return(fit$estimates)
	}
	stopCluster(superman)
	
	superman = makeCluster(ncores, type=cluster)
	registerDoSNOW(superman)
	cat("Cluster summary\n", getDoParRegistered(), "\n", getDoParName(), "\n", getDoParWorkers(), "\n", getDoParVersion(), "\n") 
	clusterSetupRNG(superman, sed=rep(6,6))
	results2 = foreach(sim=1:nsim, .combine="rbind") %dopar% {
		alpha = -0.2
		psi = seq(-.2,.6,.005)
		require("reshape")
		require("magrittr")
		require("rpsftm")
		require("survival")
		## RANDOMLY DRAWS FROM GP AND ASSIGNS PT SPECIFIC SURVIVAL ##
		O = do.call("rbind", lapply(1:n, function(j) {
							male = rbinom(1, 1,.5)
							age = rpois(1, lambda=12) + 18
							if(male==1) {
								lambda.gender = US.M
							} else {
								lambda.gender = US.F
							}
							lambda = lambda.gender[(age+1):length(lambda.gender)]
							genData = suppressWarnings(rpsftm::generateData(1, lambda=lambda, time=100, p=0.5, alpha=alpha, c=.01, crossover=0.02, long=TRUE)$data)
							genData$id = j; genData$male = male; genData$age = age
							return(genData)
						}))
		O$Y[is.na(O$Y)] = 0
		O %<>% subset(time==1, select=c("id", "A")) %>% rename(c(A="R")) %>% merge(O, by="id")
		## SUBSET OUT ANALYSIS DATA (ASSUMES MONTHLY TIME INTERVALS) ##
		dates = data.frame(dateStart=rep(seq(from=as.Date("2011-01-01"), to=as.Date("2011-01-01")+n/10*30.5, by="month"), 10))
		dates$id = seq.int(1:nrow(dates))
		O = merge(O, dates, by="id", all.x=TRUE)
		O$dateEnd = O$dateStart + (365.25/12)*O$T
		O$date = O$dateStart + (365.25/12)*(O$time-1)
		dateAnalyzed = with(subset(O, time==1), getAnalysisDate(dateEnd, delta, eventsDesired))
		analysisData = subset(O, date < dateAnalyzed)
		## UPDATE CENSORING ##
		analysisData$eofu = analysisData$dateEnd>=dateAnalyzed
		analysisData$delta = ifelse(analysisData$eofu, 0, analysisData$delta)
		analysisData$Y = ifelse(analysisData$eofu, 0, analysisData$Y)
		analysisData$T = ifelse(analysisData$eofu, ceiling(as.numeric(dateAnalyzed-analysisData$dateStart)/(365.25/12)), analysisData$T)
		analysisData$C = ifelse(analysisData$delta==1, ceiling(as.numeric(dateAnalyzed-analysisData$dateStart)/(365.25/12)), analysisData$T)
		## GET ESTIMATES ##
		analysisData$t = 1
		fit = rpsftm(T=analysisData$t, A=analysisData$A, R=analysisData$R, C=analysisData$C, Y=analysisData$delta, id=analysisData$id, psi=psi)
		return(fit$estimates)
	}
	stopCluster(superman)
	
	ggest = rbind(results1[,"Log-rank",drop=F], results1[,"Wilcoxon", drop=F], results1[,"Cox-Wald", drop=F], results1[,"Cox-HR", drop=F])
	gggrp = c(rep("Log-rank", nrow(results1)), rep("Wilcoxon", nrow(results1)), rep("Cox-Wald", nrow(results1)), rep("Cox-HR", nrow(results1)))
	ggdata = data.frame(estimates=as.numeric(ggest), grp=gggrp)
	p1 = ggplot(ggdata, aes(x=estimates, colour = grp)) +
			geom_density(size=1.5) +
			geom_vline(xintercept=-alpha, colour="red", size=1.5) +
			scale_x_continuous(limits = c(min(psi), max(psi))) +
			labs(colour = "", title=expression(paste(alpha, "= 0.2")), x=expression(widehat(psi)), y="Density") +
			theme(legend.justification=c(0,0), legend.position="top")
	ggest = rbind(results2[,"Log-rank",drop=F], results2[,"Wilcoxon", drop=F], results2[,"Cox-Wald", drop=F], results2[,"Cox-HR", drop=F])
	gggrp = c(rep("Log-rank", nrow(results2)), rep("Wilcoxon", nrow(results2)), rep("Cox-wald", nrow(results2)), rep("Cox-HR", nrow(results2)))
	ggdata = data.frame(estimates=as.numeric(ggest), grp=gggrp)
	p2 = ggplot(ggdata, aes(x=estimates, colour = grp)) +
			geom_density(size=1.5) +
			geom_vline(xintercept=alpha, colour="red", size=1.5) +
			scale_x_continuous(limits = c(-.2, .6)) +
			labs(colour = "", title=expression(paste(alpha, "= -0.2")), x=expression(widehat(psi)), y="Density") +
			theme(legend.justification=c(0,0), legend.position="top")
	multiplot(p1, p2, cols=2)
	
}



plot19 = function() {
	
	plot(NULL, xlim=c(-1,1), ylim=c(0,1), xlab=expression(hat(psi)), ylab="p-value", main="", yaxt="n")
	rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="gray90")
	abline(v=seq(-1,1,.50), col="white", lwd=1.5)
	abline(v=seq(-1,1,.25), col="white", lwd=.75)
	abline(h=seq(0,1,.20), col="white", lwd=1.5)
	abline(h=seq(0,1,.10), col="white", lwd=.75)
	axis(1, seq(-1,1,.25), labels=FALSE, las=1, tcl=-.25)
	axis(2, seq(0,1,.2), labels=TRUE, las=1)
	axis(2, seq(0,1,.1), labels=FALSE, las=1, tcl=-.25)
	with(fit$statistics, points(km.pvalue ~ psi, pch=19))
	abline(h=.05, lty=2, col=2, lwd=2)
	abline(v=fit$ci[1,1], lwd=2, col=3, lty=2)
	abline(v=fit$ci[1,2], lwd=2, col=3, lty=2)
	mtext("0.05", 4, at=.06, las=1, line=.25)
	lines(c(1.1,1.1) ~ fit$ci[1,], lwd=2, xpd=NA)
	lines(c(1.11,1.09) ~ c(fit$ci[1,1],fit$ci[1,1]), lwd=2, xpd=NA)
	lines(c(1.11,1.09) ~ c(fit$ci[1,2],fit$ci[1,2]), lwd=2, xpd=NA)
	mtext("95% CI", 3, at=-0.22, line=1)
	
}



plot20 = function(alpha = .2, psi = seq(-.6,.2,.001), n=1000, nsim=500) {
	
	superman = makeCluster(ncores, type=cluster)
	registerDoSNOW(superman)
	cat("Cluster summary\n", getDoParRegistered(), "\n", getDoParName(), "\n", getDoParWorkers(), "\n", getDoParVersion(), "\n") 
	clusterSetupRNG(superman, sed=rep(6,6))
	results = foreach(sim=1:nsim) %dopar% {
		require("rpsftm")
		require("survival")
		
		O = generateData(n=n, lambda=mx, time=120, p=0.5, alpha=alpha)$data
		O$delta = 1
		fit = rpsftm(T=O$T, A=O$A.1, R=O$A.1, C=O$T*1000, Y=O$delta, id=O$id, psi=psi)
		length=apply(fit$ci, 1, diff)
		cover = fit$ci[,1]<=-alpha & -alpha<=fit$ci[,2]
		return(cbind(est=fit$estimates, fit$ci))
	}
	stopCluster(superman)
	logrank = do.call("rbind", lapply(results, function(x) return(x[1,])))
	wilcoxon = do.call("rbind", lapply(results, function(x) return(x[2,])))
	coxwald = do.call("rbind", lapply(results, function(x) return(x[3,])))
	coxhr = do.call("rbind", lapply(results, function(x) return(x[4,])))
	
	## PLOT ##
	cols = gg_color_hue(4)
	ggest = rbind(logrank, wilcoxon, coxwald, coxhr)
	gggrp = c(rep("Log-rank", length(results)), rep("Wilcoxon", length(results)), rep("Cox-wald", length(results)), rep("Cox-HR", length(results))) 
	ggdata = data.frame(ggest, grp=gggrp)
	ggplot(ggdata, aes(x=est, colour = grp)) +
			geom_density(size=1.5) +
			scale_x_continuous(limits = c(-.6, .2)) +
			scale_y_continuous(limits = c(0, 7.5)) +
			labs(colour = "", title=expression(paste("Continuous Exp(",lambda[0],"=log(2)/10)")), x=expression(widehat(psi)), y="Density") +
			geom_segment(aes(x = -.2, y = 6, xend = -.2, yend = 0), size=1.5, col=2) +
			geom_segment(aes(x = mean(coxhr[,"lcl"]), y = 7.2, xend = mean(coxhr[,"ucl"]), yend = 7.2), size=1.5, col=cols[1]) +
			geom_segment(aes(x = mean(coxhr[,"lcl"]), y = 7.2-.1, xend = mean(coxhr[,"lcl"]), yend = 7.2+.1), size=1.5, col=cols[1]) +
			geom_segment(aes(x = mean(coxhr[,"ucl"]), y = 7.2-.1, xend = mean(coxhr[,"ucl"]), yend = 7.2+.1), size=1.5, col=cols[1]) +
			geom_segment(aes(x = mean(coxwald[,"lcl"]), y = 6.95, xend = mean(coxwald[,"ucl"]), yend = 6.95), size=1.5, col=cols[2]) +
			geom_segment(aes(x = mean(coxwald[,"lcl"]), y = 6.95-.1, xend = mean(coxwald[,"lcl"]), yend = 6.95+.1), size=1.5, col=cols[2]) +
			geom_segment(aes(x = mean(coxwald[,"ucl"]), y = 6.95-.1, xend = mean(coxwald[,"ucl"]), yend = 6.95+.1), size=1.5, col=cols[2]) +
			geom_segment(aes(x = mean(logrank[,"lcl"]), y = 6.7, xend = mean(logrank[,"ucl"]), yend = 6.7), size=1.5, col=cols[3]) +
			geom_segment(aes(x = mean(logrank[,"lcl"]), y = 6.7-.1, xend = mean(logrank[,"lcl"]), yend = 6.7+.1), size=1.5, col=cols[3]) +
			geom_segment(aes(x = mean(logrank[,"ucl"]), y = 6.7-.1, xend = mean(logrank[,"ucl"]), yend = 6.7+.1), size=1.5, col=cols[3]) +
			geom_segment(aes(x = mean(wilcoxon[,"lcl"]), y = 6.45, xend = mean(wilcoxon[,"ucl"]), yend = 6.45), size=1.5, col=cols[4]) +
			geom_segment(aes(x = mean(wilcoxon[,"lcl"]), y = 6.45-.1, xend = mean(wilcoxon[,"lcl"]), yend = 6.45+.1), size=1.5, col=cols[4]) +
			geom_segment(aes(x = mean(wilcoxon[,"ucl"]), y = 6.45-.1, xend = mean(wilcoxon[,"ucl"]), yend = 6.45+.1), size=1.5, col=cols[4]) +
			annotate("text", x = -.2, y = 7.5, label = "Average 95% CI length", size=4) +
			theme(legend.justification=c(0,0), legend.position="top")

	cat("COVERAGES\n")
	cat("Log-rank:", mean(logrank$cover))
	cat("Wilcoxon:", mean(wilcoxon$cover))
	cat("Cox-Wald:", mean(coxwald$cover))
	cat("Cox-HR:", mean(coxhr$cover))
	
	cat("LENGTHS\n")
	cat("Log-rank:", mean(logrank$length))
	cat("Wilcoxon:", mean(wilcoxon$length))
	cat("Cox-Wald:", mean(coxwald$length))
	cat("Cox-HR:", mean(coxhr$length))
	
}



plot21 = function(nsim=500, n=1000, eventsDesired=400, alpha = .2, psi = seq(-.6,.2,.001)) {
	
	superman = makeCluster(ncores, type=cluster)
	registerDoSNOW(superman)
	cat("Cluster summary\n", getDoParRegistered(), "\n", getDoParName(), "\n", getDoParWorkers(), "\n", getDoParVersion(), "\n") 
	clusterSetupRNG(superman, sed=rep(6,6))
	results = foreach(sim=1:nsim, .packages=c("rpsftm", "survival")) %dopar% {
		O = generateData(n=n, lambda=hodgkins$mx, time=120, p=0.5, alpha=alpha)$data[,c("T", "A.1", "delta", "id")]
		dates = rep(seq(from=as.Date("2011-01-01"), to=as.Date("2011-01-01")+500/10*30.5, by="month"), 10)
		O$dateStart = dates[1:nrow(O)]
		## SUBSET OUT ANALYSIS DATA ##
		O$dateEnd = O$dateStart + (365.25/12)*O$T
		dateAnalyzed = getAnalysisDate(O$dateEnd, O$delta, eventsDesired)
		analysisData = subset(O, dateStart < dateAnalyzed)
		## UPDATE CENSORING ##
		analysisData$eofu = analysisData$dateEnd>=dateAnalyzed
		analysisData$delta = ifelse(analysisData$eofu, 0, analysisData$delta)
		analysisData$T = ifelse(analysisData$eofu, ceiling(as.numeric(dateAnalyzed-analysisData$dateStart)/(365.25/12)), analysisData$T)
		analysisData$C = ifelse(analysisData$delta==1, ceiling(as.numeric(dateAnalyzed-analysisData$dateStart)/(365.25/12)), analysisData$T)
		fit = rpsftm(T=analysisData$T, A=analysisData$A.1, R=analysisData$A.1, C=analysisData$C, Y=analysisData$delta, id=analysisData$id, psi=psi)
		length=apply(fit$ci, 1, diff)
		cover = fit$ci[,1]<=-alpha & -alpha<=fit$ci[,2]
		return(cbind(est=fit$estimates, fit$ci, length=length, cover=cover))
	}
	stopCluster(superman)
	logrank = data.frame(do.call("rbind", lapply(results, function(x) return(x[1,]))))
	wilcoxon = data.frame(do.call("rbind", lapply(results, function(x) return(x[2,]))))
	coxwald = data.frame(do.call("rbind", lapply(results, function(x) return(x[3,]))))
	coxhr = data.frame(do.call("rbind", lapply(results, function(x) return(x[4,]))))
	
	## PLOT ##
	cols = gg_color_hue(4)
	ggest = rbind(logrank, wilcoxon, coxwald, coxhr)
	gggrp = c(rep("Log-rank", length(results)), rep("Wilcoxon", length(results)), rep("Cox-wald", length(results)), rep("Cox-HR", length(results))) 
	ggdata = data.frame(ggest, grp=gggrp)
	ggplot(ggdata, aes(x=est, colour = grp)) +
			geom_density(size=1.5) +
			scale_x_continuous(limits = c(-.6, .2)) +
			scale_y_continuous(limits = c(0, 5.5)) +
			labs(colour = "", title=expression(paste("Hodgkin's ",lambda["0,np"], "(t) & Censoring")), x=expression(widehat(psi)), y="Density") +
			geom_segment(aes(x = -.2, y = 4, xend = -.2, yend = 0), size=1.5, col=2) +
			geom_segment(aes(x = mean(coxhr[,"lcl"]), y = 5.2, xend = mean(coxhr[,"ucl"]), yend = 5.2), size=1.5, col=cols[1]) +
			geom_segment(aes(x = mean(coxhr[,"lcl"]), y = 5.2-.1, xend = mean(coxhr[,"lcl"]), yend = 5.2+.1), size=1.5, col=cols[1]) +
			geom_segment(aes(x = mean(coxhr[,"ucl"]), y = 5.2-.1, xend = mean(coxhr[,"ucl"]), yend = 5.2+.1), size=1.5, col=cols[1]) +
			geom_segment(aes(x = mean(coxwald[,"lcl"]), y = 4.95, xend = mean(coxwald[,"ucl"]), yend = 4.95), size=1.5, col=cols[2]) +
			geom_segment(aes(x = mean(coxwald[,"lcl"]), y = 4.95-.1, xend = mean(coxwald[,"lcl"]), yend = 4.95+.1), size=1.5, col=cols[2]) +
			geom_segment(aes(x = mean(coxwald[,"ucl"]), y = 4.95-.1, xend = mean(coxwald[,"ucl"]), yend = 4.95+.1), size=1.5, col=cols[2]) +
			geom_segment(aes(x = mean(logrank[,"lcl"]), y = 4.7, xend = mean(logrank[,"ucl"]), yend = 4.7), size=1.5, col=cols[3]) +
			geom_segment(aes(x = mean(logrank[,"lcl"]), y = 4.7-.1, xend = mean(logrank[,"lcl"]), yend = 4.7+.1), size=1.5, col=cols[3]) +
			geom_segment(aes(x = mean(logrank[,"ucl"]), y = 4.7-.1, xend = mean(logrank[,"ucl"]), yend = 4.7+.1), size=1.5, col=cols[3]) +
			geom_segment(aes(x = mean(wilcoxon[,"lcl"]), y = 4.45, xend = mean(wilcoxon[,"ucl"]), yend = 4.45), size=1.5, col=cols[4]) +
			geom_segment(aes(x = mean(wilcoxon[,"lcl"]), y = 4.45-.1, xend = mean(wilcoxon[,"lcl"]), yend = 4.45+.1), size=1.5, col=cols[4]) +
			geom_segment(aes(x = mean(wilcoxon[,"ucl"]), y = 4.45-.1, xend = mean(wilcoxon[,"ucl"]), yend = 4.45+.1), size=1.5, col=cols[4]) +
			annotate("text", x = -.2, y = 5.5, label = "Average 95% CI length", size=4) +
			theme(legend.justification=c(0,0), legend.position="top")
	
	cat("COVERAGES\n")
	cat("Log-rank:", mean(logrank$cover), "\n")
	cat("Wilcoxon:", mean(wilcoxon$cover), "\n")
	cat("Cox-Wald:", mean(coxwald$cover), "\n")
	cat("Cox-HR:", mean(coxhr$cover), "\n")
	
	cat("LENGTHS\n")
	cat("Log-rank:", mean(logrank$length), "\n")
	cat("Wilcoxon:", mean(wilcoxon$length), "\n")
	cat("Cox-Wald:", mean(coxwald$length), "\n")
	cat("Cox-HR:", mean(coxhr$length), "\n")
	
}


plot22 = function(nsim=500, n=1000, eventsDesired=400, alpha=.2, psi=seq(-.6,.2,.001), crossover=NULL) {
	
	superman = makeCluster(ncores, type=cluster)
	registerDoSNOW(superman)
	cat("Cluster summary\n", getDoParRegistered(), "\n", getDoParName(), "\n", getDoParWorkers(), "\n", getDoParVersion(), "\n") 
	clusterSetupRNG(superman, sed=rep(6,6))
	results = foreach(sim=1:nsim, .packages=c("rpsftm", "survival", "magrittr", "reshape")) %dopar% {
		## RANDOMLY DRAWS FROM GP AND ASSIGNS PT SPECIFIC SURVIVAL ##
		O = do.call("rbind", lapply(1:n, function(j) {
							r_male = rbinom(1, 1, hodgkins.Pmale)
							r_over45 = rbinom(1, 1, hodgkins.Pover45[r_male+1])
							Pstage = subset(hodgkins.Pstage, male==r_male & over45==r_over45, select=c("Pstage_12", "Pstage_3", "Pstage_4"))
							r_stage = which(rmultinom(1, 1, Pstage[1,])[,1]==1)
							lambda = subset(hodgkins.f, male==r_male & over45==r_over45 & stage==r_stage)$mx
							genData = suppressWarnings(generateData(1, lambda=lambda, time=100, p=0.5, alpha=alpha, c=.01, long=TRUE, crossover=crossover)$data)
							genData$id = j; genData$male = r_male; genData$over45 = r_over45; genData$stage = r_stage
							return(genData)
						}))
		O$Y[is.na(O$Y)] = 0
		O %<>% subset(time==1, select=c("id", "A")) %>% rename(c(A="R")) %>% merge(O, by="id")
		## SUBSET OUT ANALYSIS DATA (ASSUMES MONTHLY TIME INTERVALS) ##
		dates = data.frame(dateStart=rep(seq(from=as.Date("2011-01-01"), to=as.Date("2011-01-01")+n/10*30.5, by="month"), 10))
		dates$id = seq.int(1:nrow(dates))
		O = merge(O, dates, by="id", all.x=TRUE)
		O$dateEnd = O$dateStart + (365.25/12)*O$T
		O$date = O$dateStart + (365.25/12)*(O$time-1)
		dateAnalyzed = with(subset(O, time==1), getAnalysisDate(dateEnd, delta, eventsDesired))
		analysisData = subset(O, date < dateAnalyzed)
		## UPDATE CENSORING ##
		analysisData$eofu = analysisData$dateEnd>=dateAnalyzed
		analysisData$delta = ifelse(analysisData$eofu, 0, analysisData$delta)
		analysisData$Y = ifelse(analysisData$eofu, 0, analysisData$Y)
		analysisData$T = ifelse(analysisData$eofu, ceiling(as.numeric(dateAnalyzed-analysisData$dateStart)/(365.25/12)), analysisData$T)
		analysisData$C = ifelse(analysisData$delta==1, ceiling(as.numeric(dateAnalyzed-analysisData$dateStart)/(365.25/12)), analysisData$T)
		## ESTIMATE PARAMETER ##
		analysisData$t = 1
		fit = rpsftm(T=analysisData$t, A=analysisData$A, R=analysisData$R, C=analysisData$C, Y=analysisData$delta, id=analysisData$id, psi=psi)
		length=apply(fit$ci, 1, diff)
		cover = fit$ci[,1]<=-alpha & -alpha<=fit$ci[,2]
		return(cbind(est=fit$estimates, fit$ci, length=length, cover=cover))
	}
	stopCluster(superman)
	logrank = data.frame(do.call("rbind", lapply(results, function(x) return(x[1,]))))
	wilcoxon = data.frame(do.call("rbind", lapply(results, function(x) return(x[2,]))))
	coxwald = data.frame(do.call("rbind", lapply(results, function(x) return(x[3,]))))
	coxhr = data.frame(do.call("rbind", lapply(results, function(x) return(x[4,]))))
	
	## PLOT ##
	cols = gg_color_hue(4)
	ggest = rbind(logrank, wilcoxon, coxwald, coxhr)
	gggrp = c(rep("Log-rank", length(results)), rep("Wilcoxon", length(results)), rep("Cox-wald", length(results)), rep("Cox-HR", length(results))) 
	ggdata = data.frame(ggest, grp=gggrp)
	ggplot(ggdata, aes(x=est, colour = grp)) +
			geom_density(size=1.5) +
			scale_x_continuous(limits = c(-.6, .2)) +
			scale_y_continuous(limits = c(0, 7.5)) +
			labs(colour = "", title=expression(paste("Hodgkin's ",lambda["0,np,ijk"], "(t) & Censoring & Cross")), x=expression(widehat(psi)), y="Density") +
			geom_segment(aes(x = -.2, y = 6, xend = -.2, yend = 0), size=1.5, col=2) +
			geom_segment(aes(x = mean(coxhr[,"lcl"]), y = 7.2, xend = mean(coxhr[,"ucl"]), yend = 7.2), size=1.5, col=cols[1]) +
			geom_segment(aes(x = mean(coxhr[,"lcl"]), y = 7.2-.1, xend = mean(coxhr[,"lcl"]), yend = 7.2+.1), size=1.5, col=cols[1]) +
			geom_segment(aes(x = mean(coxhr[,"ucl"]), y = 7.2-.1, xend = mean(coxhr[,"ucl"]), yend = 7.2+.1), size=1.5, col=cols[1]) +
			geom_segment(aes(x = mean(coxwald[,"lcl"]), y = 6.95, xend = mean(coxwald[,"ucl"]), yend = 6.95), size=1.5, col=cols[2]) +
			geom_segment(aes(x = mean(coxwald[,"lcl"]), y = 6.95-.1, xend = mean(coxwald[,"lcl"]), yend = 6.95+.1), size=1.5, col=cols[2]) +
			geom_segment(aes(x = mean(coxwald[,"ucl"]), y = 6.95-.1, xend = mean(coxwald[,"ucl"]), yend = 6.95+.1), size=1.5, col=cols[2]) +
			geom_segment(aes(x = mean(logrank[,"lcl"]), y = 6.7, xend = mean(logrank[,"ucl"]), yend = 6.7), size=1.5, col=cols[3]) +
			geom_segment(aes(x = mean(logrank[,"lcl"]), y = 6.7-.1, xend = mean(logrank[,"lcl"]), yend = 6.7+.1), size=1.5, col=cols[3]) +
			geom_segment(aes(x = mean(logrank[,"ucl"]), y = 6.7-.1, xend = mean(logrank[,"ucl"]), yend = 6.7+.1), size=1.5, col=cols[3]) +
			geom_segment(aes(x = mean(wilcoxon[,"lcl"]), y = 6.45, xend = mean(wilcoxon[,"ucl"]), yend = 6.45), size=1.5, col=cols[4]) +
			geom_segment(aes(x = mean(wilcoxon[,"lcl"]), y = 6.45-.1, xend = mean(wilcoxon[,"lcl"]), yend = 6.45+.1), size=1.5, col=cols[4]) +
			geom_segment(aes(x = mean(wilcoxon[,"ucl"]), y = 6.45-.1, xend = mean(wilcoxon[,"ucl"]), yend = 6.45+.1), size=1.5, col=cols[4]) +
			annotate("text", x = -.2, y = 7.5, label = "Average 95% CI length", size=4) +
			theme(legend.justification=c(0,0), legend.position="top")
	
	cat("COVERAGES\n")
	cat("Log-rank:", mean(logrank$cover), "\n")
	cat("Wilcoxon:", mean(wilcoxon$cover), "\n")
	cat("Cox-Wald:", mean(coxwald$cover), "\n")
	cat("Cox-HR:", mean(coxhr$cover), "\n")
	
	cat("LENGTHS\n")
	cat("Log-rank:", mean(logrank$length), "\n")
	cat("Wilcoxon:", mean(wilcoxon$length), "\n")
	cat("Cox-Wald:", mean(coxwald$length), "\n")
	cat("Cox-HR:", mean(coxhr$length), "\n")
	
}



plot26 = function(n=1000, nsim=500, alpha=0.2, psi=seq(-.8,.4,.0025), crossover = 0.02) {
	
	writeLines(c(""), "log.txt")
	superman = makeCluster(ncores, type=cluster)
	registerDoSNOW(superman)
	cat("Cluster summary\n", getDoParRegistered(), "\n", getDoParName(), "\n", getDoParWorkers(), "\n", getDoParVersion(), "\n") 
	clusterSetupRNG(superman, sed=rep(6,6))
	results = foreach(sim=1:nsim, .packages=c("rpsftm", "survival", "magrittr", "reshape")) %dopar% {
		O = generateData(n, lambda=mx, time=120, p=0.5, alpha=alpha, crossover = crossover, long=TRUE)$data
		O$Y[is.na(O$Y)] = 0
		O %<>% subset(time==1, select=c("id", "A")) %>% rename(c(A="R")) %>% merge(O, by="id")
		O$C = ifelse(O$delta, O$T*1000, O$T)
		O$t = 1
		fit = rpsftm(T=O$t, A=O$A, R=O$R, C=O$C, Y=O$delta, id=O$id, psi=psi)
		length=apply(fit$ci, 1, diff)
		cover = fit$ci[,1]<=-alpha & -alpha<=fit$ci[,2]
		sink("log.txt", append=TRUE)
		cat("Sim #", sim, "\n", fit$estimates, "\n")
		sink()
		return(cbind(est=fit$estimates, fit$ci, length=length, cover=cover))
	}
	stopCluster(superman)
	logrank = do.call("rbind", lapply(results, function(x) return(x[1,])))
	wilcoxon = do.call("rbind", lapply(results, function(x) return(x[2,])))
	coxwald = do.call("rbind", lapply(results, function(x) return(x[3,])))
	coxhr = do.call("rbind", lapply(results, function(x) return(x[4,])))
	
	cols = gg_color_hue(4)
	ggest = rbind(logrank, wilcoxon, coxwald, coxhr)
	gggrp = c(rep("Log-rank", length(results)), rep("Wilcoxon", length(results)), rep("Cox-wald", length(results)), rep("Cox-HR", length(results))) 
	ggdata = data.frame(ggest, grp=gggrp)
	ggplot(ggdata, aes(x=est, colour = grp)) +
			geom_density(size=1.5) +
			scale_x_continuous(limits = c(-.6, .2)) +
			scale_y_continuous(limits = c(0, 7.5)) +
			labs(colour = "", title=expression(paste("Continuous Exp(",lambda[0],"=log(2)/10)")), x=expression(widehat(psi)), y="Density") +
			geom_segment(aes(x = -.2, y = 6, xend = -.2, yend = 0), size=1.5, col=2) +
			geom_segment(aes(x = mean(coxhr[,"lcl"]), y = 7.2, xend = mean(coxhr[,"ucl"]), yend = 7.2), size=1.5, col=cols[1]) +
			geom_segment(aes(x = mean(coxhr[,"lcl"]), y = 7.2-.1, xend = mean(coxhr[,"lcl"]), yend = 7.2+.1), size=1.5, col=cols[1]) +
			geom_segment(aes(x = mean(coxhr[,"ucl"]), y = 7.2-.1, xend = mean(coxhr[,"ucl"]), yend = 7.2+.1), size=1.5, col=cols[1]) +
			geom_segment(aes(x = mean(coxwald[,"lcl"]), y = 6.95, xend = mean(coxwald[,"ucl"]), yend = 6.95), size=1.5, col=cols[2]) +
			geom_segment(aes(x = mean(coxwald[,"lcl"]), y = 6.95-.1, xend = mean(coxwald[,"lcl"]), yend = 6.95+.1), size=1.5, col=cols[2]) +
			geom_segment(aes(x = mean(coxwald[,"ucl"]), y = 6.95-.1, xend = mean(coxwald[,"ucl"]), yend = 6.95+.1), size=1.5, col=cols[2]) +
			geom_segment(aes(x = mean(logrank[,"lcl"]), y = 6.7, xend = mean(logrank[,"ucl"]), yend = 6.7), size=1.5, col=cols[3]) +
			geom_segment(aes(x = mean(logrank[,"lcl"]), y = 6.7-.1, xend = mean(logrank[,"lcl"]), yend = 6.7+.1), size=1.5, col=cols[3]) +
			geom_segment(aes(x = mean(logrank[,"ucl"]), y = 6.7-.1, xend = mean(logrank[,"ucl"]), yend = 6.7+.1), size=1.5, col=cols[3]) +
			geom_segment(aes(x = mean(wilcoxon[,"lcl"]), y = 6.45, xend = mean(wilcoxon[,"ucl"]), yend = 6.45), size=1.5, col=cols[4]) +
			geom_segment(aes(x = mean(wilcoxon[,"lcl"]), y = 6.45-.1, xend = mean(wilcoxon[,"lcl"]), yend = 6.45+.1), size=1.5, col=cols[4]) +
			geom_segment(aes(x = mean(wilcoxon[,"ucl"]), y = 6.45-.1, xend = mean(wilcoxon[,"ucl"]), yend = 6.45+.1), size=1.5, col=cols[4]) +
			annotate("text", x = -.2, y = 7.5, label = "Average 95% CI length", size=4) +
			theme(legend.justification=c(0,0), legend.position="top")
	cat("COVERAGES\n")
	cat("Log-rank:", mean(logrank[,"cover"]), "\n")
	cat("Wilcoxon:", mean(wilcoxon[,"cover"]), "\n")
	cat("Cox-Wald:", mean(coxwald[,"cover"]), "\n")
	cat("Cox-HR:", mean(coxhr[,"cover"]), "\n")
	
	cat("LENGTHS\n")
	cat("Log-rank:", mean(logrank[,"length"]), "\n")
	cat("Wilcoxon:", mean(wilcoxon[,"length"]), "\n")
	cat("Cox-Wald:", mean(coxwald[,"length"]), "\n")
	cat("Cox-HR:", mean(coxhr[,"length"]), "\n")
	
}



plot27 = function(n=1000, nsim=500, alpha=0.2, psi=seq(-.8,.4,.0025), crossover=0.02, eventsDesired=400, mx) {
	
	writeLines(c(""), "log.txt")
	superman = makeCluster(ncores, type=cluster)
	registerDoSNOW(superman)
	cat("Cluster summary\n", getDoParRegistered(), "\n", getDoParName(), "\n", getDoParWorkers(), "\n", getDoParVersion(), "\n") 
	clusterSetupRNG(superman, sed=rep(6,6))
	results = foreach(sim=1:nsim, .packages=c("rpsftm", "survival", "magrittr", "reshape")) %dopar% {
		
		O = generateData(n, lambda=mx, time=120, p=0.5, alpha=alpha, crossover=crossover, long=TRUE)$data
		O$Y[is.na(O$Y)] = 0
		O %<>% subset(time==1, select=c("id", "A")) %>% rename(c(A="R")) %>% merge(O, by="id")
		dates = data.frame(dateStart=rep(seq(from=as.Date("2011-01-01"), to=as.Date("2011-01-01")+n/10*30.5, by="month"), 10))
		dates$id = seq.int(1:nrow(dates))
		O = merge(O, dates, by="id", all.x=TRUE)
		## SUBSET OUT ANALYSIS DATA ##
		O$dateEnd = O$dateStart + (365.25/12)*O$T
		O$date = O$dateStart + (365.25/12)*(O$time-1)
		dateAnalyzed = with(subset(O, time==1), getAnalysisDate(dateEnd, delta, eventsDesired))
		analysisData = subset(O, date < dateAnalyzed)
		## UPDATE CENSORING ##
		analysisData$eofu = analysisData$dateEnd>=dateAnalyzed
		analysisData$delta = ifelse(analysisData$eofu, 0, analysisData$delta)
		analysisData$Y = ifelse(analysisData$eofu, 0, analysisData$Y)
		analysisData$T = ifelse(analysisData$eofu, ceiling(as.numeric(dateAnalyzed-analysisData$dateStart)/(365.25/12)), analysisData$T)
		analysisData$C = ifelse(analysisData$delta==1, ceiling(as.numeric(dateAnalyzed-analysisData$dateStart)/(365.25/12)), analysisData$T)
		## ESTIMATES ##
		analysisData$t = 1
		fit = rpsftm(T=analysisData$t, A=analysisData$A, R=analysisData$R, C=analysisData$C, Y=analysisData$delta, id=analysisData$id, psi=psi)
		length=apply(fit$ci, 1, diff)
		cover = fit$ci[,1]<=-alpha & -alpha<=fit$ci[,2]
		sink("log.txt", append=TRUE)
		cat("Sim #", sim, "\n", fit$estimates, "\n")
		sink()
		return(cbind(est=fit$estimates, fit$ci, length=length, cover=cover))
	}
	stopCluster(superman)
	save(results, file="./data/plot27.RData")
	logrank = do.call("rbind", lapply(results, function(x) return(x[1,])))
	wilcoxon = do.call("rbind", lapply(results, function(x) return(x[2,])))
	coxwald = do.call("rbind", lapply(results, function(x) if(nrow(x)>2) return(x[3,])))
	coxhr = do.call("rbind", lapply(results, function(x) if(nrow(x)>2) return(x[4,])))
	
	cols = gg_color_hue(4)
	ggest = rbind(logrank, wilcoxon, coxwald, coxhr)
	gggrp = c(rep("Log-rank", nrow(logrank)), rep("Wilcoxon", nrow(wilcoxon)), rep("Cox-wald", nrow(coxwald)), rep("Cox-HR", nrow(coxhr))) 
	ggdata = data.frame(ggest, grp=gggrp)
	ggplot(ggdata, aes(x=est, colour = grp)) +
			geom_density(size=1.5) +
			scale_x_continuous(limits = c(-.8, .4)) +
			scale_y_continuous(limits = c(0, 7.5)) +
			labs(colour = "", title="", x=expression(widehat(psi)), y="Density") +
			geom_segment(aes(x = -.2, y = 6, xend = -.2, yend = 0), size=1.5, col=2) +
			geom_segment(aes(x = mean(coxhr[,"lcl"]), y = 7.2, xend = mean(coxhr[,"ucl"]), yend = 7.2), size=1.5, col=cols[1]) +
			geom_segment(aes(x = mean(coxhr[,"lcl"]), y = 7.2-.1, xend = mean(coxhr[,"lcl"]), yend = 7.2+.1), size=1.5, col=cols[1]) +
			geom_segment(aes(x = mean(coxhr[,"ucl"]), y = 7.2-.1, xend = mean(coxhr[,"ucl"]), yend = 7.2+.1), size=1.5, col=cols[1]) +
			geom_segment(aes(x = mean(coxwald[,"lcl"]), y = 6.95, xend = mean(coxwald[,"ucl"]), yend = 6.95), size=1.5, col=cols[2]) +
			geom_segment(aes(x = mean(coxwald[,"lcl"]), y = 6.95-.1, xend = mean(coxwald[,"lcl"]), yend = 6.95+.1), size=1.5, col=cols[2]) +
			geom_segment(aes(x = mean(coxwald[,"ucl"]), y = 6.95-.1, xend = mean(coxwald[,"ucl"]), yend = 6.95+.1), size=1.5, col=cols[2]) +
			geom_segment(aes(x = mean(logrank[,"lcl"]), y = 6.7, xend = mean(logrank[,"ucl"]), yend = 6.7), size=1.5, col=cols[3]) +
			geom_segment(aes(x = mean(logrank[,"lcl"]), y = 6.7-.1, xend = mean(logrank[,"lcl"]), yend = 6.7+.1), size=1.5, col=cols[3]) +
			geom_segment(aes(x = mean(logrank[,"ucl"]), y = 6.7-.1, xend = mean(logrank[,"ucl"]), yend = 6.7+.1), size=1.5, col=cols[3]) +
			geom_segment(aes(x = mean(wilcoxon[,"lcl"]), y = 6.45, xend = mean(wilcoxon[,"ucl"]), yend = 6.45), size=1.5, col=cols[4]) +
			geom_segment(aes(x = mean(wilcoxon[,"lcl"]), y = 6.45-.1, xend = mean(wilcoxon[,"lcl"]), yend = 6.45+.1), size=1.5, col=cols[4]) +
			geom_segment(aes(x = mean(wilcoxon[,"ucl"]), y = 6.45-.1, xend = mean(wilcoxon[,"ucl"]), yend = 6.45+.1), size=1.5, col=cols[4]) +
			annotate("text", x = -.2, y = 7.5, label = "Average 95% CI length", size=4) +
			theme(legend.justification=c(0,0), legend.position="top")
	cat("COVERAGES\n")
	cat("Log-rank:", mean(logrank[,"cover"]), "\n")
	cat("Wilcoxon:", mean(wilcoxon[,"cover"]), "\n")
	cat("Cox-Wald:", mean(coxwald[,"cover"]), "\n")
	cat("Cox-HR:", mean(coxhr[,"cover"]), "\n")
	
	cat("LENGTHS\n")
	cat("Log-rank:", mean(logrank[,"length"]), "\n")
	cat("Wilcoxon:", mean(wilcoxon[,"length"]), "\n")
	cat("Cox-Wald:", mean(coxwald[,"length"]), "\n")
	cat("Cox-HR:", mean(coxhr[,"length"]), "\n")
	
}



plot28 = function(n=1000, nsim=500, alpha=0.2, psi=seq(-.8,.4,.0025), crossover=0.02, eventsDesired=400) {
	
	writeLines(c(""), "log.txt")
	superman = makeCluster(ncores, type=cluster)
	registerDoSNOW(superman)
	cat("Cluster summary\n", getDoParRegistered(), "\n", getDoParName(), "\n", getDoParWorkers(), "\n", getDoParVersion(), "\n") 
	clusterSetupRNG(superman, sed=rep(6,6))
	results = foreach(sim=1:nsim, .packages=c("rpsftm", "survival", "magrittr", "reshape")) %dopar% {
		
		## RANDOMLY DRAWS FROM GP AND ASSIGNS PT SPECIFIC SURVIVAL ##
		O = do.call("rbind", lapply(1:n, function(j) {
							r_male = rbinom(1, 1, hodgkins.Pmale)
							r_over45 = rbinom(1, 1, hodgkins.Pover45[r_male+1])
							Pstage = subset(hodgkins.Pstage, male==r_male & over45==r_over45, select=c("Pstage_12", "Pstage_3", "Pstage_4"))
							r_stage = which(rmultinom(1, 1, Pstage[1,])[,1]==1)
							lambda = subset(hodgkins.f, male==r_male & over45==r_over45 & stage==r_stage)$mx
							genData = suppressWarnings(generateData(1, lambda=lambda, time=100, p=0.5, alpha=alpha, c=.01, long=TRUE, crossover=crossover)$data)
							genData$id = j; genData$male = r_male; genData$over45 = r_over45; genData$stage = r_stage
							return(genData)
						}))
		O$Y[is.na(O$Y)] = 0
		O %<>% subset(time==1, select=c("id", "A")) %>% rename(c(A="R")) %>% merge(O, by="id")
		## SUBSET OUT ANALYSIS DATA ##
		dates = data.frame(dateStart=rep(seq(from=as.Date("2011-01-01"), to=as.Date("2011-01-01")+n/10*30.5, by="month"), 10))
		dates$id = seq.int(1:nrow(dates))
		O = merge(O, dates, by="id", all.x=TRUE)
		O$dateEnd = O$dateStart + (365.25/12)*O$T
		O$date = O$dateStart + (365.25/12)*(O$time-1)
		dateAnalyzed = with(subset(O, time==1), getAnalysisDate(dateEnd, delta, eventsDesired))
		analysisData = subset(O, date < dateAnalyzed)
		## UPDATE CENSORING ##
		analysisData$eofu = analysisData$dateEnd>=dateAnalyzed
		analysisData$delta = ifelse(analysisData$eofu, 0, analysisData$delta)
		analysisData$Y = ifelse(analysisData$eofu, 0, analysisData$Y)
		analysisData$T = ifelse(analysisData$eofu, ceiling(as.numeric(dateAnalyzed-analysisData$dateStart)/(365.25/12)), analysisData$T)
		analysisData$C = ifelse(analysisData$delta==1, ceiling(as.numeric(dateAnalyzed-analysisData$dateStart)/(365.25/12)), analysisData$T)
		## ESTIMATES ##
		analysisData$t = 1
		fit = rpsftm(T=analysisData$t, A=analysisData$A, R=analysisData$R, C=analysisData$C, Y=analysisData$delta, id=analysisData$id, psi=psi)
		length=apply(fit$ci, 1, diff)
		cover = fit$ci[,1]<=-alpha & -alpha<=fit$ci[,2]
		sink("log.txt", append=TRUE)
		cat(fit$estimates, "\n")
		sink()
		return(cbind(est=fit$estimates, fit$ci, length=length, cover=cover))
	}
	stopCluster(superman)
	save(results, file="./data/plot28.RData")
	logrank = do.call("rbind", lapply(results, function(x) return(x[1,])))
	wilcoxon = do.call("rbind", lapply(results, function(x) return(x[2,])))
	coxwald = do.call("rbind", lapply(results, function(x) if(nrow(x)>2) return(x[3,])))
	coxhr = do.call("rbind", lapply(results, function(x) if(nrow(x)>2) return(x[4,])))
	
	cols = gg_color_hue(4)
	ggest = rbind(logrank, wilcoxon, coxwald, coxhr)
	gggrp = c(rep("Log-rank", nrow(logrank)), rep("Wilcoxon", nrow(wilcoxon)), rep("Cox-wald", nrow(coxwald)), rep("Cox-HR", nrow(coxhr))) 
	ggdata = data.frame(ggest, grp=gggrp)
	ggplot(ggdata, aes(x=est, colour = grp)) +
			geom_density(size=1.5) +
			scale_x_continuous(limits = c(-.8, .4)) +
			scale_y_continuous(limits = c(0, 7.5)) +
			labs(colour = "", title="", x=expression(widehat(psi)), y="Density") +
			geom_segment(aes(x = -.2, y = 6, xend = -.2, yend = 0), size=1.5, col=2) +
			geom_segment(aes(x = mean(coxhr[,"lcl"]), y = 7.2, xend = mean(coxhr[,"ucl"]), yend = 7.2), size=1.5, col=cols[1]) +
			geom_segment(aes(x = mean(coxhr[,"lcl"]), y = 7.2-.1, xend = mean(coxhr[,"lcl"]), yend = 7.2+.1), size=1.5, col=cols[1]) +
			geom_segment(aes(x = mean(coxhr[,"ucl"]), y = 7.2-.1, xend = mean(coxhr[,"ucl"]), yend = 7.2+.1), size=1.5, col=cols[1]) +
			geom_segment(aes(x = mean(coxwald[,"lcl"]), y = 6.95, xend = mean(coxwald[,"ucl"]), yend = 6.95), size=1.5, col=cols[2]) +
			geom_segment(aes(x = mean(coxwald[,"lcl"]), y = 6.95-.1, xend = mean(coxwald[,"lcl"]), yend = 6.95+.1), size=1.5, col=cols[2]) +
			geom_segment(aes(x = mean(coxwald[,"ucl"]), y = 6.95-.1, xend = mean(coxwald[,"ucl"]), yend = 6.95+.1), size=1.5, col=cols[2]) +
			geom_segment(aes(x = mean(logrank[,"lcl"]), y = 6.7, xend = mean(logrank[,"ucl"]), yend = 6.7), size=1.5, col=cols[3]) +
			geom_segment(aes(x = mean(logrank[,"lcl"]), y = 6.7-.1, xend = mean(logrank[,"lcl"]), yend = 6.7+.1), size=1.5, col=cols[3]) +
			geom_segment(aes(x = mean(logrank[,"ucl"]), y = 6.7-.1, xend = mean(logrank[,"ucl"]), yend = 6.7+.1), size=1.5, col=cols[3]) +
			geom_segment(aes(x = mean(wilcoxon[,"lcl"]), y = 6.45, xend = mean(wilcoxon[,"ucl"]), yend = 6.45), size=1.5, col=cols[4]) +
			geom_segment(aes(x = mean(wilcoxon[,"lcl"]), y = 6.45-.1, xend = mean(wilcoxon[,"lcl"]), yend = 6.45+.1), size=1.5, col=cols[4]) +
			geom_segment(aes(x = mean(wilcoxon[,"ucl"]), y = 6.45-.1, xend = mean(wilcoxon[,"ucl"]), yend = 6.45+.1), size=1.5, col=cols[4]) +
			annotate("text", x = -.2, y = 7.5, label = "Average 95% CI length", size=4) +
			theme(legend.justification=c(0,0), legend.position="top")
	cat("COVERAGES\n")
	cat("Log-rank:", mean(logrank[,"cover"]), "\n")
	cat("Wilcoxon:", mean(wilcoxon[,"cover"]), "\n")
	cat("Cox-Wald:", mean(coxwald[,"cover"]), "\n")
	cat("Cox-HR:", mean(coxhr[,"cover"]), "\n")
	
	cat("LENGTHS\n")
	cat("Log-rank:", mean(logrank[,"length"]), "\n")
	cat("Wilcoxon:", mean(wilcoxon[,"length"]), "\n")
	cat("Cox-Wald:", mean(coxwald[,"length"]), "\n")
	cat("Cox-HR:", mean(coxhr[,"length"]), "\n")
	
}



plot29 = function(n=25000, mx=hodgkins$mx, alpha=.2) {
	
	O_0 = generateData(n=n, lambda=mx, time=120, p=0, alpha=alpha)
	O_1 = generateData(n=n, lambda=mx, time=120, p=1, alpha=alpha)
	kmfit = survfit(Surv(T,delta)~A.1, data=rbind(O_0$data[,c("T", "A.1", "delta")], O_1$data[,c("T", "A.1", "delta")]))
	ggNpData = data.frame(time=kmfit$time, surv=kmfit$surv, grp=c(rep("Control", kmfit$strata[1]), rep("Treatment", kmfit$strata[2])))
	p1 = ggplot(ggNpData, aes(x=time, y=surv, colour = grp)) +
			scale_x_continuous(limits = c(0, 100)) +
			geom_line(size=1.5) + 
			labs(colour = "TX group", title=expression(paste("Survival (", alpha, "=0.2; n=25,000)", sep="")), x="Years", y=expression(paste(hat("S")["n"]))) +
			theme(legend.justification=c(0,0), legend.position=c(0.68,0.75))
	O_0.hazards = apply(O_0$lambdaOBS, 2, mean)
	O_1.hazards = apply(O_1$lambdaOBS, 2, mean)
	ggLData = data.frame(time=rep(c(0:119),2), haz=c(O_0.hazards, O_1.hazards), grp=rep(c("Control","Treatment"), each=120))
	p2 = ggplot(ggLData, aes(x=time, y=haz, colour = grp)) +
			scale_x_continuous(limits = c(0, 100)) +
			geom_line(size=1.5) + 
			labs(colour = "TX group", title=expression(paste("Hazard (", alpha, "=0.2; n=25,000)", sep="")), x="Years", y=expression(paste(hat(lambda)["n"]))) +
			theme(legend.justification=c(0,0), legend.position=c(0.68,.75))
	multiplot(p1, p2, cols=2)
	
}



plot30 = function(n=25000, mx=hodgkins$mx, alpha=.2, true=FALSE) {
	
	O_0 = generateData(n=n, lambda=mx, time=120, p=0, alpha=alpha)
	O_1 = generateData(n=n, lambda=mx, time=120, p=1, alpha=alpha)
	T = c(O_0$data$T, O_1$data$T)
	O_0.hazards = apply(O_0$lambdaOBS, 2, mean)
	O_1.hazards = apply(O_1$lambdaOBS, 2, mean)
	prob = prop.table(table(T))
	hr = data.frame(hr=O_1.hazards / O_0.hazards, time=c(0:119), prob=prob[1:120]/sum(prob[1:120]))
	coxdata = rbind(O_0$data[,c("T", "delta", "A.1")], O_1$data[,c("T", "delta", "A.1")])
	coxfit = coxph(Surv(T, delta) ~ A.1, data=coxdata)
	beta = coef(coxfit)
	if(true) {
		ggplot(hr, aes(x=time, y=hr)) +
				scale_x_continuous(limits = c(0, 100)) +
				geom_line(size=1.5) + 
				geom_hline(yintercept=exp(beta), colour="red", size=1.5) +
				annotate("text", label = "beta[0]", x = 97, y = exp(beta)+.03, size = 6, colour = "red", parse=TRUE) +
				labs(title=expression(paste("Hazard Ratio (", alpha, "=0.2; n=10,000)", sep="")), x="Years", y="Ratio")
	} else {
		ggplot(hr, aes(x=time, y=hr)) +
				scale_x_continuous(limits = c(0, 100)) +
				geom_line(size=1.5) + 
				labs(title=expression(paste("Hazard Ratio (", alpha, "=0.2; n=10,000)", sep="")), x="Years", y="Ratio")
	}
	
}



table1 = function(alpha=0.2, n=1000, nsim=500, mx=hodgkins$mx) {
	
	superman = makeCluster(ncores, type=cluster)
	registerDoSNOW(superman)
	cat("Cluster summary\n", getDoParRegistered(), "\n", getDoParName(), "\n", getDoParWorkers(), "\n", getDoParVersion(), "\n") 
	clusterSetupRNG(superman, sed=rep(6,6))
	results = foreach(sim=1:nsim, .packages=c("rpsftm", "survival", "magrittr", "reshape")) %dopar% {
		
		O = generateData(n, lambda=mx, time=120, p=0.5, alpha=alpha)$data
		coxfit = coxph(Surv(T, delta) ~ A.1, data=O)
		hr = summary(coxfit)$coef
		return(hr)
	}
	stopCluster(superman)
	tmp = do.call("rbind", results)
	
	load(paste("./data/coxBeta", alpha, ".RData", sep=""))
	cat("True beta:", beta, "\n")
	cat("Bias:", mean(tmp[,"coef"]) - beta, "\n")
	cat("MSE:", mean((tmp[,"coef"] - beta)^2), "\n")
	cat("Var:", var(tmp[,"coef"]), "\n")
	cat("sqrt(Var):", sqrt(var(tmp[,"coef"])), "\n")
	cat("mean SE:", mean(tmp[,"se(coef)"]), "\n")
	lcl = tmp[,"coef"] - qnorm(.975)*tmp[,"se(coef)"]
	ucl = tmp[,"coef"] + qnorm(.975)*tmp[,"se(coef)"]
	cat("Cov:", mean(ifelse(lcl <= beta & beta <= ucl, 1, 0)), "\n")
	
}



table2 = function(n=1000, nsim=500, alpha=0.2, psi=seq(-1,.6, .005), eventsDesired=400, mx=hodgkins$mx, crossover=0, file=NULL) {
	
	set.seed(100)
	writeLines(c(""), "log.txt")
	superman = makeCluster(ncores, type=cluster)
	registerDoSNOW(superman)
	cat("Cluster summary\n", getDoParRegistered(), "\n", getDoParName(), "\n", getDoParWorkers(), "\n", getDoParVersion(), "\n") 
	clusterSetupRNG(superman, sed=rep(6,6))
	results = foreach(sim=1:nsim, .packages=c("rpsftm", "survival", "magrittr", "reshape")) %dopar% {
		
		O = generateData(n, lambda=mx, time=120, p=0.5, alpha=alpha, crossover=crossover, long=TRUE)$data
		O$Y[is.na(O$Y)] = 0
		O %<>% subset(time==1, select=c("id", "A")) %>% rename(c(A="R")) %>% merge(O, by="id")
		dates = data.frame(dateStart=rep(seq(from=as.Date("2011-01-01"), to=as.Date("2011-01-01")+n/10*30.5, by="month"), 10))
		dates$id = seq.int(1:nrow(dates))
		O = merge(O, dates, by="id", all.x=TRUE)
		## SUBSET OUT ANALYSIS DATA ##
		O$dateEnd = O$dateStart + (365.25/12)*O$T
		O$date = O$dateStart + (365.25/12)*(O$time-1)
		dateAnalyzed = with(subset(O, time==1), getAnalysisDate(dateEnd, delta, eventsDesired))
		analysisData = subset(O, date < dateAnalyzed)
		## UPDATE CENSORING ##
		analysisData$eofu = analysisData$dateEnd>=dateAnalyzed
		analysisData$delta = ifelse(analysisData$eofu, 0, analysisData$delta)
		analysisData$Y = ifelse(analysisData$eofu, 0, analysisData$Y)
		analysisData$T = ifelse(analysisData$eofu, ceiling(as.numeric(dateAnalyzed-analysisData$dateStart)/(365.25/12)), analysisData$T)
		analysisData$C = ifelse(analysisData$delta==1, ceiling(as.numeric(dateAnalyzed-analysisData$dateStart)/(365.25/12)), analysisData$T)
		## ESTIMATES ##
		analysisData$t = 1
		fit = rpsftm(T=analysisData$t, A=analysisData$A, R=analysisData$R, C=analysisData$C, Y=analysisData$delta, id=analysisData$id, psi=psi)
		length=apply(fit$ci, 1, diff)
		Rgroup = subset(analysisData, time==1, select=c("id", "R"))
		rownames(Rgroup) = Rgroup$id
		Cxgroup = tapply(analysisData$A, analysisData$id, max)
		Rgroup$Cx[names(Cxgroup)] = Cxgroup
		probCx = mean(Rgroup$R != Rgroup$Cx)
		## HRs ##
		X_psi = fit$counterfact$X_psi[,paste("psi_", fit$estimates[[1]], sep="")]
		Y_psi = fit$counterfact$Y_psi[,paste("psi_", fit$estimates[[1]], sep="")]
		coxData = cbind(unique(subset(analysisData, select=c("id", "T", "delta", "R"))), X_psi, Y_psi)
		coxData$T = ifelse(coxData$R==0, coxData$X_psi, coxData$T)
		coxData$delta = ifelse(coxData$R==0, coxData$Y_psi, coxData$delta)
		coxfit = coxph(Surv(T, delta) ~ R, data=coxData)
		hr = cbind(summary(coxfit)$coef, logrankPhi=exp(-fit$estimates[[1]]), cx=probCx, aftlength=length)
		## OUTPUTS ##
		sink("log.txt", append=TRUE)
		cat(hr, "\n")
		sink()
		return(hr)
	}
	stopCluster(superman)
	save(results, file=file)
	hr = do.call("rbind", results)
	load(paste("./data/coxBeta", alpha, ".RData", sep=""))
	cat("True beta:", beta, "\n")
	cat("Bias:", mean(hr[,"coef"]) - beta, "\n")
	cat("MSE:", mean((hr[,"coef"] - beta)^2), "\n")
	cat("Var:", var(hr[,"coef"]), "\n")
	cat("sqrt(Var):", sqrt(var(hr[,"coef"])), "\n")
	cat("mean SE:", mean(hr[,"se(coef)"]), "\n")
	lcl = hr[,"coef"] - qnorm(.975)*hr[,"se(coef)"]
	ucl = hr[,"coef"] + qnorm(.975)*hr[,"se(coef)"]
	cat("Old Cov:", mean(ifelse(lcl <= beta & beta <= ucl, 1, 0)), "\n")
	newSE = hr[,"aftlength"] / (2*qnorm(.975))
	cat("mean SE2:", mean(newSE), "\n")
	lcl = hr[,"coef"] - qnorm(.975)*newSE
	ucl = hr[,"coef"] + qnorm(.975)*newSE
	cat("New Cov:", mean(ifelse(lcl <= beta & beta <= ucl, 1, 0)), "\n")
	
}



plot31 = function() {
	plot(NULL, xlim=c(-1,1), ylim=c(0,4), xlab=expression(hat(beta)["n"]), ylab="Density", main="", yaxt="n", xaxt="n")
	rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="gray90")
	abline(v=seq(-1,1,.5), col="white", lwd=1.5)
	abline(v=seq(-1,1,.5/2), col="white", lwd=.75)
	abline(h=seq(0,4,1), col="white", lwd=1.5)
	abline(h=seq(0,4,1/2), col="white", lwd=.75)
	axis(1, seq(-1,1,.5), labels=TRUE, las=1, tcl=-.25)
	axis(1, seq(-1,1,.5/2), labels=FALSE, las=1, tcl=-.25)
	axis(2, seq(0,4,1), labels=TRUE, las=1)
	axis(2, seq(0,4,1/2), labels=FALSE, las=1, tcl=-.25)
	for(i in 0:4) {
		load(file=paste("./data/table2-", i, ".RData", sep=""))
		hr = do.call("rbind", results)
		lines(density(hr[,"coef"]), lwd=2, col=i+1)
	}
	legend(.125, 4, c("No crossover", "P(Crossover)=0.14", "P(Crossover)=0.22", "P(Crossover)=0.32", "P(Crossover)=0.38"), fill=c(1:5), bg="white", cex=.8)
}



plot32 = function(file="cross") {
	
	plot(NULL, xlim=c(-1,.6), ylim=c(0,4), xlab=expression(hat(alpha)["n"]), ylab="Density", main="", yaxt="n", xaxt="n")
	rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="gray90")
	abline(v=seq(-1,.6,.2), col="white", lwd=1.5)
	abline(v=seq(-1,.6,.2/2), col="white", lwd=.75)
	abline(h=seq(0,4,1), col="white", lwd=1.5)
	abline(h=seq(0,4,1/2), col="white", lwd=.75)
	axis(1, seq(-1,.6,.2), labels=TRUE, las=1, tcl=-.25)
	axis(1, seq(-1,.6,.2/2), labels=FALSE, las=1, tcl=-.25)
	axis(2, seq(0,4,1), labels=TRUE, las=1)
	axis(2, seq(0,4,1/2), labels=FALSE, las=1, tcl=-.25)
	for(i in 0:4) {
		load(paste("./data/", file, i, ".RData", sep=""))
		logrank = do.call("rbind", lapply(results, function(x) return(x[1,])))
		lines(density(logrank[,1]), col=i+1)
	}
	legend(-0.05, 4, c("No crossover", "P(Crossover)=0.14", "P(Crossover)=0.22", "P(Crossover)=0.32", "P(Crossover)=0.38"), fill=c(1:5), bg="white", cex=.8)
	abline(v=-.2, lwd=2, col="orange")
}


plot32b = function(file="table3exp-") {
	
	plot(NULL, xlim=c(-1,.6), ylim=c(0,4), xlab=expression(hat(alpha)["n"]), ylab="Density", main="", yaxt="n", xaxt="n")
	rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="gray90")
	abline(v=seq(-1,.6,.2), col="white", lwd=1.5)
	abline(v=seq(-1,.6,.2/2), col="white", lwd=.75)
	abline(h=seq(0,4,1), col="white", lwd=1.5)
	abline(h=seq(0,4,1/2), col="white", lwd=.75)
	axis(1, seq(-1,.6,.2), labels=TRUE, las=1, tcl=-.25)
	axis(1, seq(-1,.6,.2/2), labels=FALSE, las=1, tcl=-.25)
	axis(2, seq(0,4,1), labels=TRUE, las=1)
	axis(2, seq(0,4,1/2), labels=FALSE, las=1, tcl=-.25)
	for(i in 0:4) {
		load(paste("./data/", file, i, ".RData", sep=""))
		hr = do.call("rbind", results)
		lines(density(-log(hr[,6])), col=i+1)
	}
	legend(-0.05, 4, c("No crossover", "P(Crossover)=0.07", "P(Crossover)=0.12", "P(Crossover)=0.22", "P(Crossover)=0.30"), fill=c(1:5), bg="white", cex=.8)
	abline(v=-.2, lwd=2, col="orange")
}


plot33 = function() {
	
	plot(NULL, xlim=c(-3,3), ylim=c(0,.4), xlab="", ylab="Density", main="Estimator distribution", yaxt="n", xaxt="n")
	rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="gray90")
	abline(v=seq(-3,3,1), col="white", lwd=1.5)
	abline(v=seq(-3,3,1/2), col="white", lwd=.75)
	abline(h=seq(0,.4,.1), col="white", lwd=1.5)
	abline(h=seq(0,.4,.1/2), col="white", lwd=.75)
	axis(1, 0, labels=expression(beta[0]), las=1, tcl=-.25)
	lines(dnorm(seq(-3,3,.01)) ~ seq(-3,3,.01), lwd=2)
	abline(v=0, col=2, lwd=2, lty=2)
	lines(c(.1,.1) ~ c(.01,1), lwd=2)
	lines(c(.11,.09) ~ c(1,1), lwd=2)
	lines(c(.11,.09) ~ c(.01,.01), lwd=2)
	text(.5, .115, label=expression("se"[0]))
	
}



table3 = function(file, true) {
	
	load(paste("./data/", true, ".RData", sep=""))
	cat("True beta:", beta, "\n")
	for(i in 0:4) {
		cat("\nSim #", i, "\n")
		load(paste("./data/", file, i, ".RData", sep=""))
		hr = do.call("rbind", results)
		cat("P(cross):", mean(hr[,"cx"]), "\n")
		cat("Bias:", mean(hr[,"coef"]) - beta, "\n")
		cat("MSE:", mean((hr[,"coef"] - beta)^2), "\n")
		cat("sqrt(Var):", sqrt(var(hr[,"coef"])), "\n")
		cat("mean SE:", mean(hr[,"se(coef)"]), "\n")
		lcl = hr[,"coef"] - qnorm(.975)*hr[,"se(coef)"]
		ucl = hr[,"coef"] + qnorm(.975)*hr[,"se(coef)"]
		cat("Old Cov:", mean(ifelse(lcl <= beta & beta <= ucl, 1, 0)), "\n")
		newSE = hr[,"aftlength"] / (2*qnorm(.975)) / -log(1-hr[,"cx"])
		cat("mean SE2:", mean(newSE), "\n")
		lcl = hr[,"coef"] - qnorm(.975)*newSE
		ucl = hr[,"coef"] + qnorm(.975)*newSE
		cat("New Cov:", mean(ifelse(lcl <= beta & beta <= ucl, 1, 0)), "\n")
	}
	
}



plot32c = function(file="table2-", labels=NULL, true) {
	
	plot(NULL, xlim=c(-1,.6), ylim=c(0,4), xlab=expression(hat(beta)["n"]), ylab="Density", main="", yaxt="n", xaxt="n")
	rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="gray90")
	abline(v=seq(-1,.6,.2), col="white", lwd=1.5)
	abline(v=seq(-1,.6,.2/2), col="white", lwd=.75)
	abline(h=seq(0,4,1), col="white", lwd=1.5)
	abline(h=seq(0,4,1/2), col="white", lwd=.75)
	axis(1, seq(-1,.6,.2), labels=TRUE, las=1, tcl=-.25)
	axis(1, seq(-1,.6,.2/2), labels=FALSE, las=1, tcl=-.25)
	axis(2, seq(0,4,1), labels=TRUE, las=1)
	axis(2, seq(0,4,1/2), labels=FALSE, las=1, tcl=-.25)
	for(i in 0:4) {
		load(paste("./data/", file, i, ".RData", sep=""))
		hr = do.call("rbind", results)
		lines(density(hr[,1]), col=i+1)
	}
	load(paste("./data/", true, ".RData", sep=""))
	legend(-0.05, 4, labels, fill=c(1:5), bg="white", cex=.8)
	abline(v=beta, lwd=2, col="orange")
}


plot34 = function(nsim=500) {
	set.seed(100)
	writeLines(c(""), "log.txt")
	superman = makeCluster(ncores, type=cluster)
	registerDoSNOW(superman)
	cat("Cluster summary\n", getDoParRegistered(), "\n", getDoParName(), "\n", getDoParWorkers(), "\n", getDoParVersion(), "\n") 
	clusterSetupRNG(superman, sed=rep(6,6))
	results = foreach(sim=1:nsim, .packages=c("rpsftm", "survival", "magrittr", "reshape")) %dopar% {
		
		O = generateData(n=1000, lambda=log(2)/10, time=120, p=0.5, alpha=0.2, long=TRUE)$data
		O$Y[is.na(O$Y)] = 0
		O %<>% subset(time==1, select=c("id", "A")) %>% rename(c(A="R")) %>% merge(O, by="id")
		dates = data.frame(dateStart=rep(seq(from=as.Date("2011-01-01"), to=as.Date("2011-01-01")+n/10*30.5, by="month"), 10))
		dates$id = seq.int(1:nrow(dates))
		O = merge(O, dates, by="id", all.x=TRUE)
		## SUBSET OUT ANALYSIS DATA ##
		O$dateEnd = O$dateStart + (365.25/12)*O$T
		O$date = O$dateStart + (365.25/12)*(O$time-1)
		dateAnalyzed = with(subset(O, time==1), getAnalysisDate(dateEnd, delta, eventsDesired))
		analysisData = subset(O, date < dateAnalyzed)
		## UPDATE CENSORING ##
		analysisData$eofu = analysisData$dateEnd>=dateAnalyzed
		analysisData$delta = ifelse(analysisData$eofu, 0, analysisData$delta)
		analysisData$Y = ifelse(analysisData$eofu, 0, analysisData$Y)
		analysisData$T = ifelse(analysisData$eofu, ceiling(as.numeric(dateAnalyzed-analysisData$dateStart)/(365.25/12)), analysisData$T)
		analysisData$C = ifelse(analysisData$delta==1, ceiling(as.numeric(dateAnalyzed-analysisData$dateStart)/(365.25/12)), analysisData$T)
		## ESTIMATES ##
		analysisData$t = 1
		fit = rpsftm(T=analysisData$t, A=analysisData$A, R=analysisData$R, C=analysisData$C, Y=analysisData$delta, id=analysisData$id, psi=psi)
		## HRs ##
		X_psi = fit$counterfact$X_psi[,paste("psi_", fit$estimates[[1]], sep="")]
		Y_psi = fit$counterfact$Y_psi[,paste("psi_", fit$estimates[[1]], sep="")]
		coxData = cbind(subset(analysisData, time==1, select=c("id", "T", "delta", "R")), X_psi, Y_psi)
		coxData$T = ifelse(coxData$R==0, coxData$X_psi, coxData$T)
		coxData$delta = ifelse(coxData$R==0, coxData$Y_psi, coxData$delta)
		coxfit = coxph(Surv(T, delta) ~ R, data=coxData)	
		## OUTPUTS ##
		sink("log.txt", append=TRUE)
		cat(hr, "\n")
		sink()
		
		return(c(alpha=fit$estimates[[1]], beta0=coef(coxph(Surv(T, delta) ~ A, data=subset(O, time==1)))[[1]], beta1=coef(coxph(Surv(T, delta) ~ A, data=subset(analysisData, time==1)))[[1]], beta2=coef(coxfit)[[1]]))
	}
	stopCluster(superman)
	ests = do.call('rbind', results)
	plot(NULL, xlim=c(-1,.6), ylim=c(0,6), xlab=expression(hat(beta)["n"]), ylab="Density", main="Estimated HRs", yaxt="n", xaxt="n")
	rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="gray90")
	abline(v=seq(-1,.6,.2), col="white", lwd=1.5)
	abline(v=seq(-1,.6,.2/2), col="white", lwd=.75)
	abline(h=seq(0,6,1), col="white", lwd=1.5)
	abline(h=seq(0,6,1/2), col="white", lwd=.75)
	axis(1, seq(-1,.6,.2), labels=TRUE, las=1, tcl=-.25)
	axis(1, seq(-1,.6,.2/2), labels=FALSE, las=1, tcl=-.25)
	axis(2, seq(0,6,1), labels=TRUE, las=1)
	axis(2, seq(0,6,1/2), labels=FALSE, las=1, tcl=-.25)
	lines(density(ests[,2]), lwd=2)
	lines(density(ests[,3]), lwd=2, col=3)
	lines(density(ests[,4]), lwd=2, col=4)
	load(paste("./data/coxBeta0.2.RData", sep=""))
	abline(v=beta)
	legend("topright", c("Uncensored", "Censored", "RPSFTM"), fill=c(1,3,4), bg="white")
}



plot35 = function(n=1000, nsim=1000) {
	
	writeLines(c(""), "log.txt")
	superman = makeCluster(ncores, type=cluster)
	registerDoSNOW(superman)
	cat("Cluster summary\n", getDoParRegistered(), "\n", getDoParName(), "\n", getDoParWorkers(), "\n", getDoParVersion(), "\n") 
	clusterSetupRNG(superman, sed=rep(6,6))
	results = foreach(sim=1:nsim, .packages=c("rpsftm", "survival", "magrittr", "reshape")) %dopar% {
		
		O = generateData(n=1000, lambda=log(2)/10, time=120, p=0.5, alpha=0.2, long=TRUE)$data
		O$Y[is.na(O$Y)] = 0
		O %<>% subset(time==1, select=c("id", "A")) %>% rename(c(A="R")) %>% merge(O, by="id")
		dates = data.frame(dateStart=rep(seq(from=as.Date("2011-01-01"), to=as.Date("2011-01-01")+n/10*30.5, by="month"), 10))
		dates$id = seq.int(1:nrow(dates))
		O = merge(O, dates, by="id", all.x=TRUE)
		## SUBSET OUT ANALYSIS DATA ##
		O$dateEnd = O$dateStart + (365.25/12)*O$T
		O$date = O$dateStart + (365.25/12)*(O$time-1)
		dateAnalyzed = with(subset(O, time==1), getAnalysisDate(dateEnd, delta, eventsDesired))
		analysisData = subset(O, date < dateAnalyzed)
		## UPDATE CENSORING ##
		analysisData$eofu = analysisData$dateEnd>=dateAnalyzed
		analysisData$delta = ifelse(analysisData$eofu, 0, analysisData$delta)
		analysisData$Y = ifelse(analysisData$eofu, 0, analysisData$Y)
		analysisData$T = ifelse(analysisData$eofu, ceiling(as.numeric(dateAnalyzed-analysisData$dateStart)/(365.25/12)), analysisData$T)
		analysisData$C = ifelse(analysisData$delta==1, ceiling(as.numeric(dateAnalyzed-analysisData$dateStart)/(365.25/12)), analysisData$T)
		## AFT ESTIMATES ##
		analysisData$t = O$t = 1
		fit1 = rpsftm(T=analysisData$t, A=analysisData$A, R=analysisData$R, C=analysisData$C, Y=analysisData$delta, id=analysisData$id, psi=psi)
		fit0 = rpsftm(T=O$t, A=O$A, R=O$A, C=ifelse(O$delta==1, 1000, O$T), Y=O$delta, id=O$id, psi=psi)
		aft = c(nocensor=fit0$estimates[[1]], censor=fit1$estimates[[1]])
		## HR ESTIMATES ##
		X_psi = fit0$counterfact$X_psi[,paste("psi_", fit0$estimates[[1]], sep="")]
		Y_psi = fit0$counterfact$Y_psi[,paste("psi_", fit0$estimates[[1]], sep="")]
		coxData = cbind(subset(O, time==1, select=c("id", "T", "delta", "R")), X_psi, Y_psi)
		coxData$T = ifelse(coxData$R==0, coxData$X_psi, coxData$T)
		coxData$delta = ifelse(coxData$R==0, coxData$Y_psi, coxData$delta)
		coxfit0 = coxph(Surv(T, delta) ~ R, data=coxData)	
		X_psi = fit1$counterfact$X_psi[,paste("psi_", fit1$estimates[[1]], sep="")]
		Y_psi = fit1$counterfact$Y_psi[,paste("psi_", fit1$estimates[[1]], sep="")]
		coxData = cbind(subset(analysisData, time==1, select=c("id", "T", "delta", "R")), X_psi, Y_psi)
		coxData$T = ifelse(coxData$R==0, coxData$X_psi, coxData$T)
		coxData$delta = ifelse(coxData$R==0, coxData$Y_psi, coxData$delta)
		coxfit1 = coxph(Surv(T, delta) ~ R, data=coxData)
		hr = c(betaAll=coef(coxph(Surv(T, delta) ~ A, data=subset(O, time==1)))[[1]], betaCensor=coef(coxph(Surv(T, delta) ~ A, data=subset(analysisData, time==1)))[[1]], betaRPAll=coef(coxfit0)[[1]], betaRPCensor=coef(coxfit1)[[1]])	
		
		## OUTPUTS ##
		sink("log.txt", append=TRUE)
		cat(c(aft, hr), "\n")
		sink()
		return(c(aft, hr))
	}
	stopCluster(superman)
	save(results, file="./data/plot35.RData")
	ests = do.call('rbind', results)
	
	par(mfcol=c(1,2))
	plot(NULL, xlim=c(-1,.6), ylim=c(0,6), xlab=expression(hat(alpha)["n"]), ylab="Density", main="Estimated AFs (n=1000)", yaxt="n", xaxt="n")
	rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="gray90")
	abline(v=seq(-1,.6,.2), col="white", lwd=1.5)
	abline(v=seq(-1,.6,.2/2), col="white", lwd=.75)
	abline(h=seq(0,6,1), col="white", lwd=1.5)
	abline(h=seq(0,6,1/2), col="white", lwd=.75)
	axis(1, seq(-1,.6,.2), labels=TRUE, las=1, tcl=-.25)
	axis(1, seq(-1,.6,.2/2), labels=FALSE, las=1, tcl=-.25)
	axis(2, seq(0,6,1), labels=TRUE, las=1)
	axis(2, seq(0,6,1/2), labels=FALSE, las=1, tcl=-.25)
	lines(density(ests[,"nocensor"]), lwd=2)
	lines(density(ests[,"censor"]), lwd=2, col=4)
	abline(v=-.2, lwd=2, col=2)
	legend("topright", c("Uncensored", "Censored (d=400)"), fill=c(1,4), bg="white", cex=.85)
	
	plot(NULL, xlim=c(-1,.6), ylim=c(0,6), xlab=expression(hat(beta)["n"]), ylab="Density", main="Estimated HRs (n=1000)", yaxt="n", xaxt="n")
	rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="gray90")
	abline(v=seq(-1,.6,.2), col="white", lwd=1.5)
	abline(v=seq(-1,.6,.2/2), col="white", lwd=.75)
	abline(h=seq(0,6,1), col="white", lwd=1.5)
	abline(h=seq(0,6,1/2), col="white", lwd=.75)
	axis(1, seq(-1,.6,.2), labels=TRUE, las=1, tcl=-.25)
	axis(1, seq(-1,.6,.2/2), labels=FALSE, las=1, tcl=-.25)
	axis(2, seq(0,6,1), labels=TRUE, las=1)
	axis(2, seq(0,6,1/2), labels=FALSE, las=1, tcl=-.25)
	lines(density(ests[,"betaAll"]), lwd=2)
	lines(density(ests[,"betaCensor"]), lwd=2, col=4)
	lines(density(ests[,"betaRPCensor"]), lwd=2, col=3)
	load(paste("./data/coxBetaExp0.2.RData", sep=""))
	abline(v=beta, lwd=2, col=2)
	legend("topright", c("Uncensored", "Censored (d=400)", "RPSFTM (d=400)"), fill=c(1,4,3), bg="white", cex=.85)
	
}


plot36 = function(alpha=.2, n=25000, mx=hodgkins[,"mx"]) {
	
	O_0 = generateData(n=n, lambda=mx, time=120, p=0, alpha=alpha)
	O_1 = generateData(n=n, lambda=mx, time=120, p=1, alpha=alpha)
	O_0.hazards = apply(O_0$lambdaOBS, 2, mean)
	O_1.hazards = apply(O_1$lambdaOBS, 2, mean)
	ggLData = data.frame(time=rep(c(0:119),2), haz=c(O_0.hazards, O_1.hazards), grp=rep(c("Control","Treatment"), each=120))
	p1 = ggplot(ggLData, aes(x=time, y=haz, colour = grp)) +
			scale_x_continuous(limits = c(0, 100)) +
			geom_line(size=1.5) + 
			labs(colour = "TX group", title=expression(paste("Hazard (", alpha, "=0.2; n=25,000)", sep="")), x="Years", y=expression(paste(lambda, "(t)"))) +
			theme(legend.justification=c(0,0), legend.position=c(0.68,.75))
	ggLData = data.frame(time=rep(c(0:119),2), ratio = O_1.hazards / O_0.hazards)
	p2 = ggplot(ggLData, aes(x=time, y=ratio)) +
			scale_x_continuous(limits = c(0, 100)) +
			geom_line(size=1.5) + 
			labs(title=expression(paste("Hazard ratio (", alpha, "=0.2; n=25,000)", sep="")), x="Years", y="HR(t)") +
			theme(legend.justification=c(0,0), legend.position=c(0.68,.75))
	multiplot(p1, p2, cols=2)
	
}



plot37 = function(n=1000, nsim=500, eventsDesired=400, alpha=0.2, psi=seq(-1,.6, .005), crossover = 0.01, dist="Exp") {
	
	writeLines(c(""), "log.txt")
	superman = makeCluster(ncores, type=cluster)
	registerDoSNOW(superman)
	cat("Cluster summary\n", getDoParRegistered(), "\n", getDoParName(), "\n", getDoParWorkers(), "\n", getDoParVersion(), "\n") 
	clusterSetupRNG(superman, sed=rep(6,6))
	results = foreach(sim=1:nsim, .packages=c("rpsftm", "survival", "magrittr", "reshape")) %dopar% {
		
		if(dist=="Exp") {
			O = generateData(n, lambda=log(2)/10, time=120, p=0.5, alpha=alpha, crossover=crossover, long=TRUE)$data
		} else if(dist=="Wei") {
			shape = 0.7
			O = generateData(n, lambda=seq(1,120)^(shape-1)*(shape/(10/log(2))^shape), time=120, p=0.5, alpha=alpha, crossover=crossover, long=TRUE)$data	
		} else if(dist=="NP") {
			O = generateData(n, lambda=hodgkins$mx, time=120, p=0.5, alpha=alpha, crossover=crossover, long=TRUE)$data
		} else if(dist=="NPUS") {
			O = generateData(n, lambda=US.mx, time=120, p=0.5, alpha=alpha, crossover=crossover, long=TRUE)$data
		}
		O$Y[is.na(O$Y)] = 0
		O %<>% subset(time==1, select=c("id", "A")) %>% rename(c(A="R")) %>% merge(O, by="id")
		dates = data.frame(dateStart=rep(seq(from=as.Date("2011-01-01"), to=as.Date("2011-01-01")+n/10*30.5, by="month"), 10))
		dates$id = seq.int(1:nrow(dates))
		O = merge(O, dates, by="id", all.x=TRUE)
		## SUBSET OUT ANALYSIS DATA ##
		O$dateEnd = O$dateStart + (365.25/12)*O$T
		O$date = O$dateStart + (365.25/12)*(O$time-1)
		dateAnalyzed = with(subset(O, time==1), getAnalysisDate(dateEnd, delta, eventsDesired))
		analysisData = subset(O, date < dateAnalyzed)
		## UPDATE CENSORING ##
		analysisData$eofu = analysisData$dateEnd>=dateAnalyzed
		analysisData$delta = ifelse(analysisData$eofu, 0, analysisData$delta)
		analysisData$Y = ifelse(analysisData$eofu, 0, analysisData$Y)
		analysisData$T = ifelse(analysisData$eofu, ceiling(as.numeric(dateAnalyzed-analysisData$dateStart)/(365.25/12)), analysisData$T)
		analysisData$C = ifelse(analysisData$delta==1, ceiling(as.numeric(dateAnalyzed-analysisData$dateStart)/(365.25/12)), analysisData$T)
		## ESTIMATES ##
		analysisData$t = 1
		fit = rpsftm(T=analysisData$t, A=analysisData$A, R=analysisData$R, C=analysisData$C, Y=analysisData$delta, id=analysisData$id, psi=psi)
		length=apply(fit$ci, 1, diff)
		Rgroup = subset(analysisData, time==1, select=c("id", "R"))
		rownames(Rgroup) = Rgroup$id
		Cxgroup = tapply(analysisData$A, analysisData$id, max)
		Rgroup$Cx[names(Cxgroup)] = Cxgroup
		probCx = mean(Rgroup$R != Rgroup$Cx)
		## HRs ##
		X_psi = fit$counterfact$X_psi[,paste("psi_", fit$estimates[[1]], sep="")]
		Y_psi = fit$counterfact$Y_psi[,paste("psi_", fit$estimates[[1]], sep="")]
		coxData = cbind(unique(subset(analysisData, select=c("id", "T", "delta", "R"))), X_psi, Y_psi)
		coxData$treated = tapply(analysisData$A, analysisData$id, max)
		coxData$T = ifelse(coxData$R!=coxData$treated, coxData$X_psi, coxData$T)
		coxData$delta = ifelse(coxData$R!=coxData$treated, coxData$Y_psi, coxData$delta)
		coxfit = coxph(Surv(T, delta) ~ R, data=coxData)
		hr = cbind(summary(coxfit)$coef, logrankPhi=exp(-fit$estimates[[1]]), cx=probCx, aftlength=length[[1]], lcl=fit$ci[[1,1]], ucl=fit$ci[[1,2]])
		## OUTPUTS ##
		sink("log.txt", append=TRUE)
		cat(hr, "\n")
		sink()
		return(hr)
	}
	stopCluster(superman)
	save(results, file=paste("./data/table4-", dist, crossover, ".RData", sep=""))
	hr = do.call("rbind", results)
	mean(hr[,"cx"])
	load(paste("./data/coxBeta", dist, "0.2.RData", sep=""))
	cat("True beta:", beta, "\n")
	cat("Bias:", mean(hr[,"coef"]) - beta, "\n")
	cat("MSE:", mean((hr[,"coef"] - beta)^2), "\n")
	cat("Var:", var(hr[,"coef"]), "\n")
	cat("sqrt(Var):", sqrt(var(hr[,"coef"])), "\n")
	cat("mean SE:", mean(hr[,"se(coef)"]), "\n")
	lcl = hr[,"coef"] - qnorm(.975)*hr[,"se(coef)"]
	ucl = hr[,"coef"] + qnorm(.975)*hr[,"se(coef)"]
	mean(ucl-lcl)
	cat("Old Cov:", mean(ifelse(lcl <= beta & beta <= ucl, 1, 0)), "\n")
	ratio = hr[,"coef"] / -log(hr[,"logrankPhi"])
	ratio = ifelse(hr[,"logrankPhi"]==1, 1, ratio)
	lcl = hr[,"lcl"]*ratio
	ucl = hr[,"ucl"]*ratio
	mean(ucl-lcl)
	cat("New Cov:", mean(ifelse(lcl <= beta & beta <= ucl, 1, 0)), "\n")
	
}



plot38 = function(n=1000, eventsDesired = 400, nsim=500, alpha=0.2, psi=seq(-1,.6, .005), crossover = 0.02, dist="Exp") {
	
	superman = makeCluster(ncores, type=cluster)
	registerDoSNOW(superman)
	cat("Cluster summary\n", getDoParRegistered(), "\n", getDoParName(), "\n", getDoParWorkers(), "\n", getDoParVersion(), "\n") 
	clusterSetupRNG(superman, sed=rep(6,6))
	results = foreach(sim=1:nsim, .packages=c("rpsftm", "survival", "magrittr", "reshape")) %dopar% {
		
		if(dist=="Exp") {
			O = generateData(n, lambda=log(2)/10, time=120, p=0.5, alpha=alpha, crossover=crossover, long=TRUE)$data
		} else if(dist=="Wei") {
			shape = 0.7
			O = generateData(n, lambda=seq(1,120)^(shape-1)*(shape/(10/log(2))^shape), time=120, p=0.5, alpha=alpha, crossover=crossover, long=TRUE)$data	
		} else if(dist=="NP") {
			O = generateData(n, lambda=hodgkins$mx, time=120, p=0.5, alpha=alpha, crossover=crossover, long=TRUE)$data
		} else if(dist=="NPUS") {
			O = generateData(n, lambda=US.mx, time=120, p=0.5, alpha=alpha, crossover=crossover, long=TRUE)$data
		}
		O$Y[is.na(O$Y)] = 0
		O %<>% subset(time==1, select=c("id", "A")) %>% rename(c(A="R")) %>% merge(O, by="id")
		dates = data.frame(dateStart=rep(seq(from=as.Date("2011-01-01"), to=as.Date("2011-01-01")+n/10*30.5, by="month"), 10))
		dates$id = seq.int(1:nrow(dates))
		O = merge(O, dates, by="id", all.x=TRUE)
		## SUBSET OUT ANALYSIS DATA ##
		O$dateEnd = O$dateStart + (365.25/12)*O$T
		O$date = O$dateStart + (365.25/12)*(O$time-1)
		dateAnalyzed = with(subset(O, time==1), getAnalysisDate(dateEnd, delta, eventsDesired))
		analysisData = subset(O, date < dateAnalyzed)
		## UPDATE CENSORING ##
		analysisData$eofu = analysisData$dateEnd>=dateAnalyzed
		analysisData$delta = ifelse(analysisData$eofu, 0, analysisData$delta)
		analysisData$Y = ifelse(analysisData$eofu, 0, analysisData$Y)
		analysisData$T = ifelse(analysisData$eofu, ceiling(as.numeric(dateAnalyzed-analysisData$dateStart)/(365.25/12)), analysisData$T)
		analysisData$C = ifelse(analysisData$delta==1, ceiling(as.numeric(dateAnalyzed-analysisData$dateStart)/(365.25/12)), analysisData$T)
		## ESTIMATES ##
		analysisData$prevA[2:nrow(analysisData)] = analysisData$A[1:(nrow(analysisData)-1)]
		analysisData$prevA[analysisData$time==1] = 0
		cxData = subset(analysisData, R==0 & prevA==0)
		tmp = glm(A ~ L, data=cxData, family="binomial")
		cxData$probA_1 = tmp$fitted.values
		analysisData = merge(analysisData, cxData[,c("id","time", "probA_1")], by=c("id", "time"), all.x=TRUE)
		analysisData$probA_1[analysisData$R==1 | analysisData$R==0 & analysisData$time==1] = mean(subset(analysisData, time==1)$R)
		analysisData$probA_1[analysisData$prevA==1] = 1
		analysisData$probA = ifelse(analysisData$A==1, analysisData$probA_1, 1-analysisData$probA_1)
		analysisData$probR = ifelse(analysisData$A==1, mean(subset(analysisData, time==1)$R), 1-mean(subset(analysisData, time==1)$R))
		analysisData$cumProbA = unlist(with(analysisData, tapply(probA, id, cumprod)))
		analysisData$unstbWts = (analysisData$R==analysisData$A)/analysisData$cumProbA
		analysisData$stbWts = (analysisData$R==analysisData$A)*analysisData$probR/analysisData$cumProbA
		analysisData$t0 = analysisData$time - 1
		
		fit1 = coxph(Surv(t0, time, delta) ~ R + cluster(id), data=analysisData)
		fit2 = coxph(Surv(t0, time, Y) ~ R + cluster(id), data=analysisData, weights=unstbWts, subset=(unstbWts>0))
		fit3 = coxph(Surv(t0, time, Y) ~ R + cluster(id), data=analysisData, weights=stbWts, subset=(stbWts>0))
		
		return(c(Naive.est=summary(fit1)$coef[[1]], Naive.se=summary(fit1)$coef[[4]], Unstb.est=summary(fit2)$coef[[1]], Unstb.se=summary(fit2)$coef[[4]], Stb.est=summary(fit3)$coef[[1]], Stb.se=summary(fit3)$coef[[4]]))
		
	}
	stopCluster(superman)
	save(results, file=paste("./data/table5-", dist, crossover, ".RData", sep=""))
	hr = do.call("rbind", results)
	load(paste("./data/coxBeta", dist, "0.2.RData", sep=""))
	cat("True beta:", beta, "\n")
	cat("Bias:", mean(hr[,"Stb.est"]) - beta, "\n")
	cat("MSE:", mean((hr[,"Stb.est"] - beta)^2), "\n")
	cat("Var:", var(hr[,"Stb.est"]), "\n")
	cat("sqrt(Var):", sqrt(var(hr[,"Stb.est"])), "\n")
	cat("mean SE:", mean(hr[,"Stb.se"]), "\n")
	lcl = hr[,"Stb.est"] - qnorm(.975)*hr[,"Stb.se"]
	ucl = hr[,"Stb.est"] + qnorm(.975)*hr[,"Stb.se"]
	mean(ucl-lcl)
	cat("Coverage:", mean(ifelse(lcl <= beta & beta <= ucl, 1, 0)), "\n")
	
}


table_t = function(n=1000, nsim=500, eventsDesired=400, alpha=0.2, crossover=.1, psi=seq(-1,.6,.005), dist="Exp") {
	
	writeLines(c(""), "log.txt")
	superman = makeCluster(ncores, type=cluster)
	registerDoSNOW(superman)
	cat("Cluster summary\n", getDoParRegistered(), "\n", getDoParName(), "\n", getDoParWorkers(), "\n", getDoParVersion(), "\n") 
	clusterSetupRNG(superman, sed=rep(6,6))
	results = foreach(sim=1:nsim, .packages=c("rpsftm", "survival", "magrittr", "reshape")) %dopar% {
		
		if(dist=="Exp") {
			O = generateData(n, lambda=log(2)/10, time=120, p=0.5, alpha=alpha, crossover=crossover, long=TRUE)$data
		} else if(dist=="Wei") {
			shape = 0.7
			O = generateData(n, lambda=seq(1,120)^(shape-1)*(shape/(10/log(2))^shape), time=120, p=0.5, alpha=alpha, crossover=crossover, long=TRUE)$data	
		} else if(dist=="NP") {
			O = generateData(n, lambda=hodgkins$mx, time=120, p=0.5, alpha=alpha, crossover=crossover, long=TRUE)$data
		} else if(dist=="NPUS") {
			O = generateData(n, lambda=US.mx, time=120, p=0.5, alpha=alpha, crossover=crossover, long=TRUE)$data
		}
		O$Y[is.na(O$Y)] = 0
		O %<>% subset(time==1, select=c("id", "A")) %>% rename(c(A="R")) %>% merge(O, by="id")
		dates = data.frame(dateStart=rep(seq(from=as.Date("2011-01-01"), to=as.Date("2011-01-01")+n/10*30.5, by="month"), 10))
		dates$id = seq.int(1:nrow(dates))
		O = merge(O, dates, by="id", all.x=TRUE)
		## SUBSET OUT ANALYSIS DATA ##
		O$dateEnd = O$dateStart + (365.25/12)*O$T
		O$date = O$dateStart + (365.25/12)*(O$time-1)
		dateAnalyzed = with(subset(O, time==1), getAnalysisDate(dateEnd, delta, eventsDesired))
		analysisData = subset(O, date < dateAnalyzed)
		## UPDATE CENSORING ##
		analysisData$eofu = analysisData$dateEnd>=dateAnalyzed
		analysisData$delta = ifelse(analysisData$eofu, 0, analysisData$delta)
		analysisData$Y = ifelse(analysisData$eofu, 0, analysisData$Y)
		analysisData$T = ifelse(analysisData$eofu, ceiling(as.numeric(dateAnalyzed-analysisData$dateStart)/(365.25/12)), analysisData$T)
		analysisData$C = ifelse(analysisData$delta==1, ceiling(as.numeric(dateAnalyzed-analysisData$dateStart)/(365.25/12)), analysisData$T)
		## WEIGHTS ##
		analysisData$prevA[2:nrow(analysisData)] = analysisData$A[1:(nrow(analysisData)-1)]
		analysisData$prevA[analysisData$time==1] = 0
		cxData = subset(analysisData, R==0 & prevA==0)
		tmp = glm(A ~ L, data=cxData, family="binomial")
		tmp2 = glm(A ~ 1, data=cxData, family="binomial")
		cxData$probA_1 = tmp$fitted.values
		cxData$probA_1num = tmp2$fitted.values
		analysisData = merge(analysisData, cxData[,c("id","time", "probA_1", "probA_1num")], by=c("id", "time"), all.x=TRUE)
		analysisData$probA_1[analysisData$R==1 | analysisData$R==0 & analysisData$time==1] = analysisData$probA_1num[analysisData$R==1 | analysisData$R==0 & analysisData$time==1] = mean(subset(analysisData, time==1)$R)
		analysisData$probA_1[analysisData$prevA==1] = analysisData$probA_1num[analysisData$prevA==1] = 1
		analysisData$probA = ifelse(analysisData$A==1, analysisData$probA_1, 1-analysisData$probA_1)
		analysisData$probAnum = ifelse(analysisData$A==1, analysisData$probA_1num, 1-analysisData$probA_1num)
		analysisData$probR = ifelse(analysisData$A==1, mean(subset(analysisData, time==1)$R), 1-mean(subset(analysisData, time==1)$R))
		analysisData$cumProbA = unlist(with(analysisData, tapply(probA, id, cumprod)))
		analysisData$cumProbAnum = unlist(with(analysisData, tapply(probAnum, id, cumprod)))
		analysisData$unstbWts = 1/analysisData$cumProbA
		analysisData$stbWts = analysisData$cumProbAnum/analysisData$cumProbA
		analysisData$t = 1
		fit = rpsftm(T=analysisData$t, A=analysisData$A, R=analysisData$R, C=analysisData$C, Y=analysisData$delta, id=analysisData$id, psi=psi, weights=analysisData$probAnum/analysisData$probA)
		af = cbind(fit$estimates, fit$ci)
		out = c(af[1,], af[3,], af[4,])
		names(out) = c("km.est", "km.lcl", "km.ucl", "wald.est", "wald.lcl", "wald.ucl", "hr.est", "hr.lcl", "hr.ucl")
		## OUTPUTS ##
		sink("log.txt", append=TRUE)
		cat(out, "\n")
		sink()
		return(out)
		
	}
	save(results, file=paste("./data/table5-", dist, crossover, ".RData", sep=""))
	
	aft = do.call("rbind", results)
	cat("Log-rank\n")
	cat("sqrt(Var):", sqrt(var(aft[,"km.est"])), "\n")
	mean(aft[,"km.ucl"]-aft[,"km.lcl"])
	cat("Coverage:", mean(ifelse(aft[,"km.lcl"] <= -alpha & -alpha <= aft[,"km.ucl"], 1, 0)), "\n")
	cat("sqrt(Var):", sqrt(var(aft[,"hr.est"])), "\n")
	mean(aft[,"hr.ucl"]-aft[,"hr.lcl"])
	cat("Coverage:", mean(ifelse(aft[,"hr.lcl"] <= -alpha & -alpha <= aft[,"hr.ucl"], 1, 0)), "\n")
	
}


table4 = function(n=1000, eventsDesired=400, nsim=500, alpha=0.2, crossover = 0.1, dist="Exp") {
	
	superman = makeCluster(ncores, type=cluster)
	registerDoSNOW(superman)
	cat("Cluster summary\n", getDoParRegistered(), "\n", getDoParName(), "\n", getDoParWorkers(), "\n", getDoParVersion(), "\n") 
	clusterSetupRNG(superman, sed=rep(6,6))
	results = foreach(sim=1:nsim, .packages=c("rpsftm", "survival", "magrittr", "reshape")) %dopar% {
		
		if(dist=="Exp") {
			O = generateData(n=n, lambda=log(2)/10, time=120, p=0.5, alpha=alpha, crossover=crossover, long=TRUE)$data
		} else if(dist=="Wei") {
			shape = 0.7
			O = generateData(n=n, lambda=seq(1,120)^(shape-1)*(shape/(10/log(2))^shape), time=120, p=0.5, alpha=alpha, crossover=crossover, long=TRUE)$data	
		} else if(dist=="NP") {
			O = generateData(n=n, lambda=hodgkins$mx, time=120, p=0.5, alpha=alpha, crossover=crossover, long=TRUE)$data
		} else if(dist=="NPUS") {
			O = generateData(n=n, lambda=US.mx, time=120, p=0.5, alpha=alpha, crossover=crossover, long=TRUE)$data
		}
		O$Y[is.na(O$Y)] = 0
		O %<>% subset(time==1, select=c("id", "A")) %>% rename(c(A="R")) %>% merge(O, by="id")
		dates = data.frame(dateStart=rep(seq(from=as.Date("2011-01-01"), to=as.Date("2011-01-01")+n/10*30.5, by="month"), 10))
		dates$id = seq.int(1:nrow(dates))
		O = merge(O, dates, by="id", all.x=TRUE)
		## SUBSET OUT ANALYSIS DATA ##
		O$dateEnd = O$dateStart + (365.25/12)*O$T
		O$date = O$dateStart + (365.25/12)*(O$time-1)
		dateAnalyzed = with(subset(O, time==1), getAnalysisDate(dateEnd, delta, eventsDesired))
		analysisData = subset(O, date < dateAnalyzed)
		analysisData$t0 = analysisData$time - 1
		## UPDATE CENSORING ##
		analysisData$eofu = analysisData$dateEnd>=dateAnalyzed
		analysisData$delta = ifelse(analysisData$eofu, 0, analysisData$delta)
		analysisData$Y = ifelse(analysisData$eofu, 0, analysisData$Y)
		analysisData$T = ifelse(analysisData$eofu, ceiling(as.numeric(dateAnalyzed-analysisData$dateStart)/(365.25/12)), analysisData$T)
		analysisData$C = ifelse(analysisData$delta==1, ceiling(as.numeric(dateAnalyzed-analysisData$dateStart)/(365.25/12)), analysisData$T)
		## g-FIT ##
		analysisData$prevA[2:nrow(analysisData)] = analysisData$A[1:(nrow(analysisData)-1)]
		analysisData$prevA[analysisData$time==1] = 0
		cxData = subset(analysisData, R==0 & prevA==0 & time>1)
		gAWFit = glm(A ~ L, data=cxData, family="binomial")
		gAFit = glm(A ~ 1, data=cxData, family="binomial")
		cxData$probA_1 = gAWFit$fitted.values
		cxData$probA_1num = gAFit$fitted.values
		analysisData = merge(analysisData, cxData[,c("id","time", "probA_1", "probA_1num")], by=c("id", "time"), all.x=TRUE)
		analysisData$probA_1[analysisData$R==1 | analysisData$R==0 & analysisData$time==1] = analysisData$probA_1num[analysisData$R==1 | analysisData$R==0 & analysisData$time==1] = mean(subset(analysisData, time==1)$R)
		analysisData$probA_1[analysisData$prevA==1] = analysisData$probA_1num[analysisData$prevA==1] = 1
		analysisData$probA = ifelse(analysisData$A==1, analysisData$probA_1, 1-analysisData$probA_1)
		analysisData$probAnum = ifelse(analysisData$A==1, analysisData$probA_1num, 1-analysisData$probA_1num)
		analysisData$probR = ifelse(analysisData$A==1, mean(subset(analysisData, time==1)$R), 1-mean(subset(analysisData, time==1)$R))
		analysisData$cumProbA = unlist(with(analysisData, tapply(probA, id, cumprod)))
		analysisData$cumProbAnum = unlist(with(analysisData, tapply(probAnum, id, cumprod)))
		## IPCW WEIGHTS ##
		analysisData$unstbWts_1 = (analysisData$R==analysisData$A)/analysisData$cumProbA
		analysisData$stbWts_1 = (analysisData$R==analysisData$A)*analysisData$cumProbAnum/analysisData$cumProbA	
		## IPTW WEIGHTS ##
		analysisData$unstbWts_2 = 1/analysisData$cumProbA
		analysisData$stbWts_2 = analysisData$cumProbAnum/analysisData$cumProbA
		## ESTIMATES ##
		fit1 = coxph(Surv(t0, time, Y) ~ R + cluster(id), data=analysisData, weights=unstbWts_1, subset=(unstbWts_1>0))
		fit1b = coxph(Surv(t0, time, Y) ~ R + cluster(id), data=analysisData, weights=stbWts_1, subset=(unstbWts_1>0))
		fit2 = coxph(Surv(t0, time, Y) ~ R + I(A!=R) + cluster(id), data=analysisData, weights=unstbWts_2)
		fit2b = coxph(Surv(t0, time, Y) ~ R + I(A!=R) + cluster(id), data=analysisData, weights=stbWts_2)
		
		out = c(summary(fit1)$coef[,c(1,3,4)], summary(fit1b)$coef[,c(1,3,4)], summary(fit2)$coef[1,c(1,3,4)], summary(fit2)$coef[2,c(1,3,4)], summary(fit2b)$coef[1,c(1,3,4)], summary(fit2b)$coef[2,c(1,3,4)])
		names(out) = c("ipcw.Runstb.coef", "ipcw.Runstb.NaiveSE", "ipcw.Runstb.RobustSE", "ipcw.Rstb.coef", "ipcw.Rstb.NaiveSE", "ipcw.Rstb.RobustSE", "iptw.Runstb.coef", "iptw.Runstb.NaiveSE", "iptw.Runstb.RobustSE", "iptw.ARunstb.coef", "iptw.ARunstb.NaiveSE", "iptw.ARunstb.RobustSE", "iptw.Rstb.coef", "iptw.Rstb.NaiveSE", "iptw.Rstb.RobustSE", "iptw.ARstb.coef", "iptw.ARstb.NaiveSE", "iptw.ARstb.RobustSE")
		
		return(out)
		
	}
	stopCluster(superman)
	save(results, file=paste("./data/table8-", dist, crossover, ".RData", sep=""))
	#load(file=paste("./data/table8-", dist, crossover, ".RData", sep=""))
	
	load(paste("./data/coxBeta", dist, "0.2.RData", sep=""))
	cat("True beta:", beta, "\n")
	hr = do.call("rbind", results)
	
	## STABLE WEIGHTS ##
	printEst(hr, "ipcw.Rstb.coef", "ipcw.Rstb.RobustSE", beta)
	printEst(hr, "iptw.Rstb.coef", "iptw.Rstb.RobustSE", beta)
	printEst(hr, "iptw.ARstb.coef", "iptw.ARstb.RobustSE", beta)
	
	lcl = hr[,"iptw.ARstb.coef"] - qnorm(.975)*hr[,"iptw.ARstb.RobustSE"]
	ucl = hr[,"iptw.ARstb.coef"] + qnorm(.975)*hr[,"iptw.ARstb.RobustSE"]
	mean(ifelse(lcl<= .75*beta & .75*beta <= ucl, 1, 0))
	
}


table5 = function(n=1000, eventsDesired=400, nsim=200, alpha=0.2, crossover = 0.1, dist="Exp") {
	
	superman = makeCluster(ncores, type=cluster)
	registerDoSNOW(superman)
	cat("Cluster summary\n", getDoParRegistered(), "\n", getDoParName(), "\n", getDoParWorkers(), "\n", getDoParVersion(), "\n") 
	clusterSetupRNG(superman, sed=rep(6,6))
	results = foreach(sim=1:nsim, .packages=c("rpsftm", "survival", "magrittr", "reshape")) %dopar% {
		
		if(dist=="Exp") {
			O = generateData(n, lambda=log(2)/10, time=120, p=0.5, alpha=alpha, crossover=crossover, long=TRUE)$data
		} else if(dist=="Wei") {
			shape = 0.7
			O = generateData(n, lambda=seq(1,120)^(shape-1)*(shape/(10/log(2))^shape), time=120, p=0.5, alpha=alpha, crossover=crossover, long=TRUE)$data	
		} else if(dist=="NP") {
			O = generateData(n, lambda=hodgkins$mx, time=120, p=0.5, alpha=alpha, crossover=crossover, long=TRUE)$data
		} else if(dist=="NPUS") {
			O = generateData(n, lambda=US.mx, time=120, p=0.5, alpha=alpha, crossover=crossover, long=TRUE)$data
		}
		O$Y[is.na(O$Y)] = 0
		O %<>% subset(time==1, select=c("id", "A")) %>% rename(c(A="R")) %>% merge(O, by="id")
		dates = data.frame(dateStart=rep(seq(from=as.Date("2011-01-01"), to=as.Date("2011-01-01")+n/10*30.5, by="month"), 10))
		dates$id = seq.int(1:nrow(dates))
		O = merge(O, dates, by="id", all.x=TRUE)
		## SUBSET OUT ANALYSIS DATA ##
		O$dateEnd = O$dateStart + (365.25/12)*O$T
		O$date = O$dateStart + (365.25/12)*(O$time-1)
		dateAnalyzed = with(subset(O, time==1), getAnalysisDate(dateEnd, delta, eventsDesired))
		analysisData = subset(O, date < dateAnalyzed)
		analysisData$t0 = analysisData$time - 1
		## UPDATE CENSORING ##
		analysisData$eofu = analysisData$dateEnd>=dateAnalyzed
		analysisData$delta = ifelse(analysisData$eofu, 0, analysisData$delta)
		analysisData$Y = ifelse(analysisData$eofu, 0, analysisData$Y)
		analysisData$T = ifelse(analysisData$eofu, ceiling(as.numeric(dateAnalyzed-analysisData$dateStart)/(365.25/12)), analysisData$T)
		analysisData$C = ifelse(analysisData$delta==1, ceiling(as.numeric(dateAnalyzed-analysisData$dateStart)/(365.25/12)), analysisData$T)
		## g-FIT ##
		analysisData$prevA[2:nrow(analysisData)] = analysisData$A[1:(nrow(analysisData)-1)]
		analysisData$prevA[analysisData$time==1] = 0
		cxData = subset(analysisData, R==0 & prevA==0 & time>1)
		gAWFit = glm(A ~ W1 + W2 + L, data=cxData, family="binomial")
		gAFit_old = glm(A ~ 1, data=cxData, family="binomial")
		gAFit = glm(A ~ W1 + W2, data=cxData, family="binomial")
		cxData$probA_1den = gAWFit$fitted.values
		cxData$probA_1num = gAFit$fitted.values
		cxData$probA_1num_old = gAFit_old$fitted.values
		analysisData = merge(analysisData, cxData[,c("id","time", "probA_1den", "probA_1num", "probA_1num_old")], by=c("id", "time"), all.x=TRUE)
		analysisData$probA_1den[analysisData$R==1 | analysisData$R==0 & analysisData$time==1] = analysisData$probA_1num[analysisData$R==1 | analysisData$R==0 & analysisData$time==1] = analysisData$probA_1num_old[analysisData$R==1 | analysisData$R==0 & analysisData$time==1] = mean(subset(analysisData, time==1)$R)
		analysisData$probA_1den[analysisData$prevA==1] = analysisData$probA_1num[analysisData$prevA==1] = analysisData$probA_1num_old[analysisData$prevA==1] = 1
		analysisData$probAden = ifelse(analysisData$A==1, analysisData$probA_1den, 1-analysisData$probA_1den)
		analysisData$probAnum = ifelse(analysisData$A==1, analysisData$probA_1num, 1-analysisData$probA_1num)
		analysisData$probAnum_old = ifelse(analysisData$A==1, analysisData$probA_1num_old, 1-analysisData$probA_1num_old)
		analysisData$probR = ifelse(analysisData$A==1, mean(subset(analysisData, time==1)$R), 1-mean(subset(analysisData, time==1)$R))
		analysisData$cumProbAden = unlist(with(analysisData, tapply(probAden, id, cumprod)))
		analysisData$cumProbAnum = unlist(with(analysisData, tapply(probAnum, id, cumprod)))
		analysisData$cumProbAnum_old = unlist(with(analysisData, tapply(probAnum_old, id, cumprod)))
		## IPCW WEIGHTS ##
		analysisData$unstbWts_1 = (analysisData$R==analysisData$A)/analysisData$cumProbAden
		analysisData$stbWts_1_old1 = (analysisData$R==analysisData$A)*analysisData$probR/analysisData$cumProbAden
		analysisData$stbWts_1_old2 = (analysisData$R==analysisData$A)*analysisData$cumProbAnum_old/analysisData$cumProbAden
		analysisData$stbWts_1 = (analysisData$R==analysisData$A)*analysisData$cumProbAnum/analysisData$cumProbAden
		## ESTIMATES ##
		fit1 = coxph(Surv(t0, time, Y) ~ R + cluster(id), data=analysisData, weights=unstbWts_1, subset=(unstbWts_1>0))
		fit2 = coxph(Surv(t0, time, Y) ~ R + cluster(id), data=analysisData, weights=stbWts_1_old1, subset=(unstbWts_1>0))
		fit3 = coxph(Surv(t0, time, Y) ~ R + cluster(id), data=analysisData, weights=stbWts_1_old2, subset=(unstbWts_1>0))
		fit4 = coxph(Surv(t0, time, Y) ~ R + cluster(id), data=analysisData, weights=stbWts_1, subset=(unstbWts_1>0))
		
		out = c(summary(fit1)$coef[,c(1,3,4)], summary(fit2)$coef[,c(1,3,4)], summary(fit3)$coef[,c(1,3,4)], summary(fit4)$coef[,c(1,3,4)])
		names(out) = c("ipcw.Runstb.coef", "ipcw.Runstb.NaiveSE", "ipcw.Runstb.RobustSE", "ipcw.Rstb.coef", "ipcw.Rstb.NaiveSE", "ipcw.Rstb.RobustSE", "ipcw.Rstb2.coef", "ipcw.Rstb2.NaiveSE", "ipcw.Rstb2.RobustSE", "ipcw.Rstb3.coef", "ipcw.Rstb3.NaiveSE", "ipcw.Rstb3.RobustSE")
		
		return(out)
		
	}
	stopCluster(superman)
	
	save(results, file="table5-comp.RData")
	hrs = do.call("rbind", results)
	
	plot(density(hrs[,1]), lwd=2, ylim=c(0,2.5))
	abline(v=-.2, col=2, lwd=2)
	lines(density(hrs[,4]), lwd=2, col=3)
	lines(density(hrs[,7]), lwd=2, col=4)
	lines(density(hrs[,10]), lwd=2, col=5)
	
}
