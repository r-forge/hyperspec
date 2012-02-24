pkgs <- available.packages ()
types <- c ("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")
revdep <- lapply (types,
                  function (type) rownames (pkgs) [grepl ("ggplot2", pkgs [, type])])
names (revdep) <- types
revdep
packageDescription ()

read.dcf ("")
