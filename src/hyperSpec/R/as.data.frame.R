##’ Conversion of a hyperSpec object into a data.frame or matrix
##’ \code{as.data.frame} returns \code{x@data} (as data.frame) \code{as.matrix}
##’ returns the spectra matrix \code{x@data$spc} as matrix
##’ 
##’ \code{as.long.df} returns a long-format data.frame \code{as.wide.df}
##’ returns a wide-format data.frame, i.e. expands the spectra matrix into a
##’ data.frame, see details.
##’ 
##’ The data.frame returned by \code{as.long.df} is guaranteed to have columns
##’ \code{spc} and \code{.wavelength}. If \code{nwl (x) == 0} these columns
##’ will be \code{NA}.
##’ 
##’ \code{as.wide.df} converts the spectra matrix to a data.frame. The extra
##’ data together with this data is returned. The column names of the spectra
##’ matrix are retained (if they are numbers, without preceeding letters).
##’ 
##’ @name as.data.frame-methods
##’ @aliases as.data.frame as.long.df as.wide.df as.t.df as.data.frame-methods
##’   as.data.frame,hyperSpec,missing,missing-method
##’   as.data.frame,hyperSpec-method as.matrix as.matrix-methods
##’   as.matrix,ANY-method as.matrix,hyperSpec-method
##’ @docType methods
##’ @param x a \code{hyperSpec} object
##’ @param c("row.names,optional, ignored", "list() ignored")
##’ @param rownames should the rownames be in column \code{.rownames} of the
##’   long-format data.frame?
##’ @param wl.factor should the wavelengths be returned as a factor (instead of
##’   numeric)?
##’ @param na.rm if \code{TRUE}, rows where spc is not \code{NA} are deleted.
##’ @return \code{x@data} and \code{x@data$spc}, respectively.
##’ 
##’ \code{as.long.df} the stacked or molten version of \code{x@data}. The
##’   wavelengths are in column \code{.wavelength}.
##’ 
##’ \code{as.wide.df} returns a data.frame that consists of the extra data and
##’   the spectra matrix converted to a data.frame. The spectra matrix is
##’   expanded \emph{in place}.
##’ 
##’ \code{as.t.df} returns a data.frame similar to \code{as.long.df}, but each
##’   spectrum in its own column. This is useful for exporting summary spectra,
##’   see the example.
##’ @author C. Beleites
##’ @seealso \code{\link[base]{as.data.frame}} and
##’   \code{\link[base]{as.matrix}}
##’ 
##’ \code{\link[hyperSpec:extract_replace]{[}}
##’ 
##’ \code{\link[utils]{stack}} and \code{\link[reshape]{melt}} for other
##’   functions producing long-format data.frames.
##’ @keywords methods
##’ @examples
##’ 
##’ as.data.frame (chondro [1:3,, 600:620])
##’ as.matrix (chondro [1:3,, 600:620])
##’ 
##’ as.long.df (flu [,, 405 ~ 410])
##’ summary (as.long.df (flu [,, 405 ~ 410]))
##’ summary (as.long.df (flu [,, 405 ~ 410], rownames = TRUE))
##’ summary (as.long.df (flu [,, 405 ~ 410], wl.factor = TRUE))
##’ 
##’ as.wide.df (chondro [1:5,, 600 ~ 610])
##’ summary (as.wide.df (chondro [1:5,, 600 ~ 610]))
##’ 
##’ df <- as.t.df (apply (chondro, 2, mean_pm_sd))
##’ head (df)
##’ 
##’ if (require (ggplot2)){
##’   ggplot (df, aes (x = .wavelength)) +
##’     geom_ribbon (aes (ymin = mean.minus.sd, ymax = mean.plus.sd),
##’       fill = "#00000040") +
##’     geom_line (aes (y = mean))
##’ }
##’ 
setMethod ("as.data.frame",
           signature (x = "hyperSpec", row.names = "missing", optional = "missing"),
           function (x, ...){
             validObject (x)

             x@data
           })

