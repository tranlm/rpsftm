# Description: Calculates treatment hazards based on AFT assumption
# 
# Author: Linh Tran
# Date: May 11, 2015
###############################################################################



aftLambda = function(lambda, time, alpha) {
	S_0.t = rep(1,length(time)+1)
	for(i in 1:length(time)) {
		S_0.t[i+1] = S_0.t[i]*exp(-lambda[i])
	}
	if(min(time)!=1) {
		time = time - min(time) +1
	}
	S_acc = cbind(time=seq.int(min(time),max(time))*exp(alpha), surv=S_0.t[2:length(S_0.t)])
	S_acc = rbind(c(0,1),S_acc)
	lambda.tx = rep(NA, length(lambda))
	for(i in 0:length(time)) {
		interval = max(which(S_acc[,"time"]<=i))
		if(interval!=nrow(S_acc)) {
			lambda.tx[i+1] = -log(S_acc[interval+1,"surv"]/S_acc[interval,"surv"]) / (S_acc[interval+1,"time"] - S_acc[interval,"time"])
		} else {
			lambda.tx[i+1] = exp(alpha)*lambda[i]
		}
	}
	return(lambda.tx[1:length(lambda)])
}

