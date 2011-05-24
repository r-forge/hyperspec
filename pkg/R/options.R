.options <- list (log = TRUE,           # for .logentry
                  debuglevel = 0L,      # for spc.identify, map.identify
                  gc = FALSE            # frequent calling of gc in read.ENVI & initialize
                  )

hy.getOptions <- function (...){
  dots <- c (...)
  if (length (dots) == 0L)
    .options
  else
  .options [dots]
}

hy.getOption <- function (name){
  .options [[name]]
}

hy.setOptions <- function (...){
  new <- list (...)
  names <- nzchar (names (new))

  if (! all (names))
    warning ("options without name are discarded: ", which (! names))
  
  opts <- modifyList (.options, new [names])
  
  if (sys.parent() == 0) 
    env <- asNamespace ("hyperSpec")
  else
    env <- parent.frame ()

  assign(".options", opts, envir = env)

  invisible (opts)
}

