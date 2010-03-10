##' xyplot as panel function for levelplot
##' 
##' levelplot like plotting but using dots instead of rectangles.
##' 
##' @aliases panel.leveldotplot
##' @author C. Beleites
##' @seealso \code{\link[lattice]{levelplot}}, \code{\link[lattice]{panel.xyplot}}
##' @export 
##' @callGraph
##' @keywords dplot
##' @examples 
##' plotmap (chondro, clusters ~ x * y, panel = panel.leveldotplot)
##' @log C. Beleites 2010-03-10
##' @title xyplot as panel function for levelplot
##' @param x, y, z see  \code{\link[lattice]{panel.levelplot}}
##' @param at, col.regions see  \code{\link[lattice]{panel.levelplot}}
##' @param ... handed to \code{\link[lattice]{panel.xyplot}}
##' @param pch handed to \code{\link[lattice]{panel.xyplot}}
##' @return a lattice object
##' @author Claudia Beleites

panel.leveldotplot <- function (x, y, z, at = pretty(z), ..., col.regions = regions$col, alpha.regions = regions$alpha, pch = 20) 
{
    regions <- trellis.par.get("regions")
    z <- as.numeric(z)
    zcol <- level.colors(z, at, col.regions, colors = TRUE)
    panel.xyplot (x, y, col = zcol, ..., pch = pch)
}
