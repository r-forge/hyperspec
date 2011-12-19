library (hyperSpec)

jdx <- readLines ("shimadzu.jdx")

spcstart <-  grep ("^##XYDATA= [(]XY[.][.]XY[)][\\s]*$", jdx) + 1
spcend <- grep ("^##END=[\\s]*$", jdx) - 1
spcstart
spcend

metastart <- c (1, head (spcend, -1) + 1)

stopifnot (length (spcstart) == length (spcend))
stopifnot (all (spcstart < spcend))

spc <- list ()
for (s in seq_along (spcstart)){
  ## read.txt.long produces hyperSpec object
  spc [[s]] <- read.txt.long  (textConnection (jdx [spcstart [s] : spcend [s]]),
                               cols = list(.wavelength = "m/z", spc = "I / a.u."),
                               header = FALSE, sep = ",")

  ## look for metadata
  meta <- jdx [metastart [s] : (spcstart [s] - 2)]

  CASname <- grepl ("##CAS NAME=(.*)$", meta)
  if (any (CASname))
    spc[[s]]$CASname <- gsub ("##CAS NAME=(.*)$", "\\1", meta [CASname]) 

  ## ...
}
spc <- collapse (spc)

spc
plot (spc, lines.args = list (type = "h"), stacked = TRUE)
spc$..
