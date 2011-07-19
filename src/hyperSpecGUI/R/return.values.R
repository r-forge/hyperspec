
# The following does not do what we need
# I have read about 'hacks' for pass by reference
# We don't want to do that though do we?

myObject <- 'object'
mySpikes <- 'blank'

myGUI <- function (someObject, someSpikes) {
  
  someSpikes <<- 'changed'
  
  ## only modal dialogs can return data
  ## modal dialogs in gWidgets are widgets and not very customisable
  return ()
}

myGUI(myObject, mySpikes)

print(mySpikes)