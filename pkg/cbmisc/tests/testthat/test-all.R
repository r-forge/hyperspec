context ("ATTACHED TESTS")
names <- ls (envir = getNamespace("cbmisc"), all.names = TRUE)

tests <- lapply (names, function (n) {attr (get (n), "test")})
tests <- tests [! sapply (tests, is.null)]
lapply (tests, eval)
context ("END OF ATTACHED TESTS")
