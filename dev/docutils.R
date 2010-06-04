load ("~/hyperspec.rforge/Vignettes/introduction/functions.RData")


## find all exported functions
ns <- asNamespace ("hyperSpec")

new.functions <- data.frame (name = ls (envir = ns), group = NA, method = FALSE, description = NA)

## find all exported methods
tmp <- getGenerics (where = ns)@.Data
new.functions <- rbind (functions,
                        data.frame (name = tmp, group = NA, method = TRUE, description = ""))

tmp <- match (functions$name, new.functions$name)

cat ("vanished functions:\n", paste (functions$name [is.na (tmp)], collapse = "\n"), "\n\n")


functions <- merge (functions, new.functions, all.y = TRUE, all.x = TRUE)

save (functions, file = "~/hyperspec.rforge/Vignettes/introduction/functions.RData")


make.fn.table <- function (){
load ("~/hyperspec.rforge/Vignettes/introduction/functions.RData")
functions <- subset (functions, !internal)
functions$group <- functions$group[,drop=TRUE]

TeX.escape <- function (x){
#  x <- gsub ("^\\\\([^\\\\])", "\\\\\\\\\\1", x)
#  x <- gsub ("[^\\\\]\\\\$", "\\1\\\\\\\\", x)
  x <- gsub ("([^\\\\]|^)\\$", "\\1\\\\$", x)
  x <- gsub ("([^\\\\]|^)_", "\\1\\\\_", x)
  x <- gsub ("([^\\\\]|^)%", "\\1\\\\%", x)
  x
}

cat('
\\begin{table*}
\\noindent\\caption{Further functions implemented by \\phy. \\emph{Emphasized} names indicate related functions provided by other \\R packages (package name before the colons).}\\label{tab:functions}
\\begin{small}
\\renewcommand{\\arraystretch}{1.5}
\\begin{tabular}{>{\\raggedright}p{0.3\\textwidth}p{0.65\\textwidth}}
\\toprule
\\textbf{Function}                                                          & \\textbf{Explanation}\\\\\\cmidrule(lr){1-1}\\cmidrule(lr){2-2}
')
for (g in levels (functions$group)){
  cat ("\\multicolumn{2}{\\emph{",g, "}}\\\\\n", sep = "")
  df <- t (functions [functions$group == g, c ("name", "description")])
  cat (paste (TeX.escape (df[1,]), df[2,], sep = " & ", collapse = "\\\\\n"),"\n")
}
cat('
\\bottomrule
\\end{tabular}
\\end{small}
\\end{table*}
')
}
