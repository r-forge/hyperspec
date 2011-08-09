dput.to.clipboard <- function (x) {
   clipboard <- pipe ("xclip -i", "w")
   dput (x, file = clipboard)
   close (clipboard)
}

### what is the function `writeClipboard()` that supposedly works well on windows?
### see http://www.johndcook.com/r_excel_clipboard.html and many other places