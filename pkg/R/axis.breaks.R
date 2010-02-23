#################################################################################
###
###  axis.break.R - poor man's version of axis.break 
###  Time-stamp: <Claudia Beleites on Tuesday, 2010-02-23 at 21:03:31 on cb>
###  
###  
###  Version 1.0  2010-02-23 21:02  Claudia Beleites  Claudia.Beleites@gmx.de
###
#################################################################################

axis.break <- function (axis = 1,breakpos = NULL, ...) 
  mtext("//", at = breakpos, side = axis, padj = -1, adj = 0.5)
