require(gWidgets)

gui <- function(data) {
  out <- character()
  w <- gbasicdialog("My Modal",
                      handler=function(h,...) {
                      #out <<- 'success'
                    })
  g <- ggroup(cont=w)
  obj <- gbutton("Different", container = g, handler = function(...){ out <<- 'different' })
  
  out = 'value' 
  visible(w, set=TRUE)
  out
}