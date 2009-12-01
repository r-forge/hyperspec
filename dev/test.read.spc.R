# Tests for read.spc
# 
# Author: C. Beleites
###############################################################################

setwd ("~/Uni/Projekte/hyperspec.rforge/dev/spc Sample Data")

spc <- read.spc ("BARBITUATES.SPC")

.print.digest.check (spc)

# BENZENE.SPC :
spc <- read.spc ("BENZENE.SPC")

# CONTOUR.SPC :
try (spc <- read.spc ('CONTOUR.SPC'))
## old format, stops coorectly with error

# DEMO 3D.SPC 
try (spc <- read.spc ('DEMO 3D.SPC'))
## old format, stops coorectly with error

# DRUG SAMPLE_PEAKS.SPC 
spc <- read.spc ('DRUG SAMPLE_PEAKS.SPC')

# DRUG SAMPLE.SPC 
spc <- read.spc ('DRUG SAMPLE.SPC')

# FID.SPC 
spc <- read.spc ('FID.SPC')

# HCL.SPC 
spc <- read.spc ('HCL.SPC')
## TODO: Transmittance immer in % ?

# HOLMIUM.SPC 
spc <- read.spc ('HOLMIUM.SPC')
# IG_BKGND.SPC 
spc <- read.spc ('IG_BKGND.SPC')
# IG_MULTI.SPC 
spc <- read.spc ('IG_MULTI.SPC')
# IG_SAMP.SPC 
spc <- read.spc ('IG_SAMP.SPC')

# KKSAM.SPC 
spc <- read.spc ('KKSAM.SPC')
# LC DIODE ARRAY.SPC 
try (spc <- read.spc ('LC DIODE ARRAY.SPC'))
# old file format

# POLYR.SPC 
spc <- read.spc ('POLYR.SPC')
# POLYS.SPC 
spc <- read.spc ('POLYS.SPC')
# SINGLE POLYMER FILM.SPC 
spc <- read.spc ('SINGLE POLYMER FILM.SPC')
# SPECTRUM WITH BAD BASELINE.SPC 
spc <- read.spc ('SPECTRUM WITH BAD BASELINE.SPC')
# TOLUENE.SPC 
spc <- read.spc ('TOLUENE.SPC')
# TUMIX.SPC 
spc <- read.spc ('TUMIX.SPC')
# TWO POLYMER FILMS.SPC 
spc <- read.spc ('TWO POLYMER FILMS.SPC')
# XYTRACE.SPC 
spc <- read.spc ('XYTRACE.SPC')


setwd ("~/Uni/Projekte/hyperspec.rforge/dev/spc Sample Data")
spc <- read.spc ('~/Uni/Projekte/hyperspec.rforge/dev/Kaiser/Map 20090921 180944/ebroAU16.spc')


files <- dir ('~/Uni/Projekte/hyperspec.rforge/dev/Kaiser/Map 20090921 180944', '*.spc', full.names = TRUE)
length (files)
files <- files [1:10]

system.time (low <- read.spc.KaiserMap(files [seq (1, length (files), by = 2)], glob = FALSE))
system.time (high <- read.spc.KaiserMap(files [seq (2, length (files), by = 2)], glob = FALSE))

# je ca. 15 - 30 s
plot (low, "spcprctl5")
plot (high, "spcprctl5")

files <- dir ('~/Uni/Projekte/hyperspec.rforge/dev/Kaiser/Map 20090921 180944', '*.spc')
system.time (for (i in 1 : 100) spc <- readspc ("~/Uni/Projekte/hyperspec.rforge/dev/Kaiser/Map 20090921 180944/", files [i]))


Rprof ()
files <- dir ('~/Uni/Projekte/hyperspec.rforge/dev/Kaiser/Map 20090921 180944', '*.spc', full.names = TRUE)
system.time (for (i in 1 : 1000) spc <- read.spc (files [i], no.object = TRUE))
Rprof(NULL)
summaryRprof()


