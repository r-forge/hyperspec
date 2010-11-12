unloadNamespace ("hyperSpec")
source ("~/hyperSpec.rforge/dev/fix.R")


library (hyperSpec)

tmp <- chondro

`[.hyperSpec` <- function (x, i, j, l, ...,
                                       wl.index = FALSE,
                                       short = "[]", date = NULL, user = NULL,
                                       drop = FALSE # drop has to be at end
                                       ){
  validObject (x)

  if (drop)
    warning ("Ignoring drop = TRUE.")

  x <- .extract (x, i, j, l, ..., wl.index = wl.index)

  if (is.null (x@data$spc)){
    x@data$spc <- matrix (NA, nrow (x@data), 0)
    x@wavelength <- numeric (0)
  }

  .logentry (x, short = short,
             long = .call.list (match.call (call = sys.call (-1))),
             date = date, user = user)
}

`[.hyperSpec`





Rprof ()
system.time (
   for (i in 1 : 1000)
      .extract(chondro, 213)
)
Rprof (NULL)


library (proftools)

summaryRprof("Rprof.out")

flatProfile(readProfileData("Rprof.out"), FALSE)
plotProfileCallGraph (readProfileData("Rprof.out"), FALSE)






RSiteSearch ("speed profiling ")
library (profr)

glm_ex <- profr(
                for (i in 1 : 1000)
      chondro [213])
glm_ex
df <- subset (glm_ex, leaf == TRUE)

tmp <- aggregate (df$time, by = list (f = df$f, level = df$level), FUN = sum)






summary(glm_ex)

plot(glm_ex)
