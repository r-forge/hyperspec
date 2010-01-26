#################################################################################
###
###  internal.R - hyperSpec internal functions
###  Time-stamp: <Claudia Beleites on Tuesday, 2010-01-26 at 16:08:47 on cb>
###  
###  
###  Version 1.0  2010-01-26 11:16  Claudia Beleites  Claudia.Beleites@gmx.de
###
#################################################################################

###-----------------------------------------------------------------------------
###
### generate a list of function arguments for the calling function
###

.call.list <- function (x = NULL) {
  if (is.null (x))
    x <- sys.call (-1)

  if (length (x) < 3)
    I (list ())
  else {
    x <- as.list (x [- (1 : 2)])
    I (x)
  }
}
###-------------------------------------------------------------------------------
###
###  .defaults - merge list with defaults
###
###

.defaults <- function (args, ...) {
  dots <- list (...)

  args <- args [! sapply (args, is.null)]

  argnames <- names (args)
  for (n in names (dots))
    if (n %in% argnames)
      dots [[n]] <- args [[n]]

  tmp <- ! argnames %in% names (dots)
  if (any (tmp))
    dots <- c (dots, args [[tmp]])

  dots
}

###-------------------------------------------------------------------------------
###
###  .logentry - create new log item (hyperSpec)
###
###
.logentry <- function (x, .entry = NULL, ...){
  validObject (x)

  .entry <- c (.entry, list (...))
  Call <- sys.call (-1);
  
  .entry <- .defaults (.entry, 
                      short = Call [[1]],
                      long = .call.list (Call),
                      date = Sys.time (),
                      user = paste (Sys.info()[c("user", "nodename")], collapse= "@"))
                       
  x@log <- rbind (x@log, data.frame (short.description = as.character (.entry$short),
                                     long.description = I(list (.entry$long)),
                                     date = .entry$date,
                                     user = .entry$user
                                     )
                  )

  x
}
