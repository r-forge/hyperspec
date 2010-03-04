#################################################################################
###
###  axis.break.R - poor man's version of axis.break 
###  Time-stamp: <Claudia Beleites on Thursday, 2010-03-04 at 15:18:11 on cb>
###  
###  
###  Version 1.0  2010-02-23 21:02  Claudia Beleites  Claudia.Beleites@gmx.de
###
#################################################################################

.axis.break <- function (axis = 1,breakpos = NULL, ...) 
  mtext("//", at = breakpos, side = axis, padj = -1, adj = 0.5)

