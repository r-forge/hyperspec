require(svUnit)

test(emsc) <- function(){
  
  checkTrue( is.test( emsc ))
  
  checkTrue( is.matrix( emsc( flu[[]], jitter( flu[[]][1,], 100 ) ) ) )
  
  checkEquals( dim( emsc( flu[[]], jitter( flu[[]][1,], 100 ) ) ), dim( flu[[]] ) )
  
  checkTrue( chk.hy( emsc( flu, jitter( flu[[]][1,], 100) ) ) )
  
  checkEquals( dim( emsc( flu, jitter( flu[[]][1,], 100))[[]]), dim( flu[[]] ) )
  
}

