###-----------------------------------------------------------------------------
###
### bind: cbind & rbind
###
###

bind <- function (direction = stop ("direction ('c' or 'r') required"),
                  ..., short = "bind", user = NULL, date = NULL){
  dots <- list (...)

  if ((length (dots) == 1) & is.list (dots [[1]]))
    dots <- dots[[1]]

  if (length (dots) == 0)
    NULL
  else if (length (dots) == 1){
    validObject (dots[[1]])
    dots[[1]]
  } else {                              # binding is actually needed.
    lapply (dots, chk.hy)
    lapply (dots, validObject)
    
    logs <- list()

    for (i in seq_along (dots) [-1]){
      dots[[1]] <- switch (direction,
                           c = cbind2 (dots[[1]], dots[[i]]),
                           r = rbind2 (dots[[1]], dots[[i]]),
                           stop ("direction must be either 'c' or 'r' for cbind",
                                 "and rbind, respectively.")
                           )

      dots [[1]] <- .logentry (dots [[1]], short = short,
                               long = list (direction = direction,
                                 .paste.row (dots [[i]])),
                               user = user, date = date)
      ## if (!is.null (short))
        ## dots[[1]]@log[nrow (dots[[1]]@log), "short.description"] <- paste (short, dots[[1]]@log[nrow (dots[[1]]@log), "short.description"])
      ## if (!is.null (date))
        ## dots[[1]]@log[nrow (dots[[1]]@log), "date"] <- date
      ## if (!is.null (user))
        ## dots[[1]]@log[nrow (dots[[1]]@log), "user"] <- user
    }
    
    dots [[1]]
  }
}

cbind.hyperSpec <- function (..., deparse.level) bind ("c", ..., short = "cbind")
rbind.hyperSpec <- function (..., deparse.level) bind ("r", ..., short = "cbind")

