dput.to.clipboard <- function (x) {
   clipboard <- pipe ("xclip -i", "w")
   dput (x, file = clipboard)
   close (clipboard)
}

### what is the function `writeClipboard()` that supposedly works well on windows?
### see http://www.johndcook.com/r_excel_clipboard.html and many other places

## on Mac, the pipe command is supposed to be pipe("pbcopy", "w"), see ? pipe, section clipboard.
## use sessionInfo ()$platform 
