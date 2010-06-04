
ns <- asNamespace ("hyperSpec")

all.fn <- ls (envir = ns, all.names = T)
all.fn <- all.fn [- grep ("__.*__", all.fn) ]

hidden.fn <- sapply (all.fn,
                     function (x){
                       tmp <- find (x, mode = "function")
                       if (length (tmp) == 0) x else NA
                     })
hidden.fn <- hidden.fn [! is.na (hidden.fn)]

for ( x in hidden.fn) 
  assign (x, get (x, envir = ns, inherits = FALSE))
