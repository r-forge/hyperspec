read.spe <- function (filename, keys.hdr2data = FALSE){
raw.data <- readBin (filename, "raw", file.info (filename)$size, 1)

.DATEMAX <- 10

hdr <- list (xDimDet        = readBin   (raw.data [7   :8              ], "integer", 1, 2, signed = FALSE), # word
             mode           = readBin   (raw.data [9   :10             ], "integer", 1, 2, signed = TRUE ), # short 
             exp_sec        = readBin   (raw.data [11  :14             ], "double",  1, 4),                 # float
             VChipXDim      = readBin   (raw.data [15  :16             ], "integer", 1, 2, signed = TRUE ), # short
             VChipYDim      = readBin   (raw.data [17  :18             ], "integer", 1, 2, signed = TRUE ), # short
             yDimDet        = readBin   (raw.data [ 19 :20             ], "integer", 1, 2, signed = FALSE), # word
             date           = rawToChar (raw.data [(1  : .DATEMAX) + 20]),                                  # char
             DetTemperature =  readBin  (raw.data [37  :40             ], "double",  1, 4),                 # float
             xdim           = readBin   (raw.data [43  :44             ], "integer", 1, 2, signed = FALSE), # word
             datatype       = readBin   (raw.data [109 :110            ], "integer", 1, 2, signed = TRUE ), # short
             ydim           = readBin   (raw.data [657 :658            ], "integer", 1, 2, signed = FALSE), # word
             lnoscan        = readBin   (raw.data [665 :6668           ], "integer", 1, 4, signed = TRUE ), # long
             NumFrames      = readBin   (raw.data [1447:1450           ], "integer", 1, 4, signed = TRUE )  # long
    )

spc <- raw.data [- (1:4100)]
spc <- switch (hdr$datatype + 1,
       readBin (spc, "double", length (spc) / 4, 4), # float
       readBin (spc, "integer", length (spc) / 4, 4, signed = TRUE), # long
       readBin (spc, "integer", length (spc) / 2, 2, signed = TRUE), # int
       readBin (spc, "integer", length (spc) / 2, 2, signed = FALSE), # uint
       )
dim (spc) <- c (hdr$xdim, hdr$ydim * hdr$NumFrames)

data <- data.frame (px.y = rep (seq_len (hdr$ydim), hdr$NumFrames),
      frame = rep (seq_len (hdr$NumFrames), each = hdr$ydim)
     )
hdr <- hdr [keys.hdr2data]
if (length (hdr > 0))
    data <- cbind (data, hdr)


new ("hyperSpec", spc = t (spc), data = as.data.frame (data))
}
