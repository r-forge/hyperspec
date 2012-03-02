hdr <- readBin (file, what = "raw", n = 1020L)

nwl <-  readBin (hdr [10:11], what = "integer", size = 2)


header <- list (
  samples <- readBin (hdr [25:26], what = "integer", size = 2)
  lines <- readBin (hdr [27:28], what = "integer", size = 2)
  )

for (i in 0 : 7)
  print (prettyNum (readBin (hdr [-seq_len (i)], what = "double", size=8, n = 1000), zero.print = "."))