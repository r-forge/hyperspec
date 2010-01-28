###-------------------------------------------------------------------------------
###
###  .defaults - merge list with defaults
###
###

.defaults <- function (args, ...) {
  dots <- list (...)

  args <- args [! sapply (args, is.null)]

  argnames <- names (args)
  for (n in names (dots))
    if (n %in% argnames)
      dots [[n]] <- args [[n]]

  tmp <- ! argnames %in% names (dots)
  if (any (tmp))
    dots <- c (dots, args [[tmp]])

  dots
}
