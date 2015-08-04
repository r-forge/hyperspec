##' Detailed listing of objects
##'
##' @param names names of the object to list
##' @param sort column to sort
##' @param total logical indicating whether the total size should be printed as well
##' @return data.frame with columns giving name, class, length, dim and size of the objects.
##' @author Claudia Beleites
##' @export
lscb <- function (names = ls (envir = parent.frame ()), sort = "size", total = TRUE){
  size <- sapply (names, function (x) (object.size (get (x))))
  if (sort == "size")
    order <- order (size)

  res <- data.frame (name = names,
                     class = sapply (names, function (x) class (get (x))),
                     length = sapply (names, function (x) length (get (x))),
                     dim = sapply (names, function (x) paste (dim (get (x)), collapse = " x ")),
                     size = .makeHumanSize (round (size))
                     )

  rownames (res) <- NULL

  if (sort != "size")
    order <- order (res [[sort]])

  res <- res [order,]
  if (total)
    res <- rbind (res, data.frame (name = "total",
                                   class = "", length = nrow (res), dim = "",
                                   size = if (nrow (res) == 0L) "0 B" else .makeHumanSize (sum (size))))

  res
}


`.test<-` <- function (name, value){
	attr (name, "test") <- value
	return (name)
}
.test (lscb) <- expression ({
	context("lscb .test")

	## TODO: Rename context
	## TODO: Add more tests

	test_that("multiplication works", {
		expect_equal(2 * 2, 4)
	})
})


.makeHumanSize <- function (x){
  exp <- floor (log (x, 1024))
  x <- x / 1024^exp
  units <- c (" B", "kB", "MB", "GB", "TB") [exp + 1]

  sprintf ("%0.0f %s", round (x), units)
}
