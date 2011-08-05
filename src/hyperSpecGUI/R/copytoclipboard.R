dput.to.clipboard <- function (x) {
   clipboard <- pipe ("xclip -i", "w")
   dput (x, file = clipboard)
   close (clipboard)
}
