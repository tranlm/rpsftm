# Description: Recreates colors from ggplot plots
# Obtained from http://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
#
# Author: Linh Tran
# Date: May 21, 2015
###############################################################################


gg_color_hue <- function(n) {
	hues = seq(15, 375, length=n+1)
	hcl(h=hues, l=65, c=100)[1:n]
}
