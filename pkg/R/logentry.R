###-----------------------------------------------------------------------------
###
###  logentry
###
###

logentry <- function (x, short = NULL, long = NULL, date = NULL, user = NULL){
  .is.hy (x)
  validObject (x)

  .logentry (x, short = short, long = long, date = date, user = user)
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
