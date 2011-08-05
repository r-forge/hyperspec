require("gWidgets")
require("cranvas")

tmp <- qdata(list('a'=c(1,2,3),'b'=c(4,5,6)))

gui <- function (data) {
  
  obj <- gbutton("Hello world", container = gwindow(), handler = function(...){ data$b[2] <-9 })
}

gui(tmp)

## tmp will change once the button has been clicked