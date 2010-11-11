.options <- list (log = TRUE)

hyperSpec.getOptions <- function (){
  .options
}

hyperSpec.getOption <- function (name){
  .options [[name]]
}

hyperSpec.setOptions <- function (...){
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

