###-----------------------------------------------------------------------------
###
### factor2num - conversion of a factor containing numerical levels
###
###


factor2num <- function (f)
	as.numeric(levels (f)) [as.numeric (f)]

