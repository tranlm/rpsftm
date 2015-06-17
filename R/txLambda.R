# Description: Assigns treatment lambda based on model chosen
# 
# Author: Linh Tran
# Date: May 11, 2015
###############################################################################


#' @export 
txLambda = function(lambda, years, alpha, type=c("accel", "propor", "additive", "ldr")) {
	type = match.arg(type)
	switch(type, 
			accel = aftLambda(lambda, years, alpha),
			propor = lambda*exp(alpha),
			additive = lambda + alpha,
			lecd = lambda*0)
}

