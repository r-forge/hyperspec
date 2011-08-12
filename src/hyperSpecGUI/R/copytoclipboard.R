dput.to.clipboard <- function (x) {
  
  ## see below for other OS tests
  
   switch (Sys.info()["sysname"],
           Linux = {
             clipboard <- pipe ("xclip -i", "w")
             dput (x, file = clipboard)
             close (clipboard)
           },
           Darwin = { ### Mac
             clipboard <- pipe ("pbcopy", "w")
             dput (x, file = clipboard)
             close (clipboard)
           },
           Windows = { ### Untested
             writeClipboard(x)
           }
          )
   
}


### what is the function `writeClipboard()` that supposedly works well on windows?
### see http://www.johndcook.com/r_excel_clipboard.html and many other places

## on Mac, the pipe command is supposed to be pipe("pbcopy", "w"), see ? pipe, section clipboard.
## use sessionInfo ()$platform 

## choices:
##  *  R.Version ()$os          == "linux-gnu" (not necessarily correct)
##  *  Sys.info ()["sysname"]   == "Linux" or "Darwin"
##  *  .Platform$OS.type        == "unix" or "windows"
##  *  sessionInfo ()$platform  == "x86_64-pc-linux-gnu (64-bit)"

