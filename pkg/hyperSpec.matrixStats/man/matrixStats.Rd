\name{matrixStats}
\alias{anyMissing}
\alias{anyMissing,hyperSpec-method}
\alias{colAlls}
\alias{colAlls,hyperSpec-method}
\alias{colAnys}
\alias{colAnys,hyperSpec-method}
\alias{colCollapse}
\alias{colCollapse,hyperSpec-method}
\alias{colCounts}
\alias{colCounts,hyperSpec-method}
\alias{colDiffs}
\alias{colDiffs,hyperSpec-method}
\alias{colIQRs}
\alias{colIQRs,hyperSpec-method}
\alias{colMads}
\alias{colMads,hyperSpec-method}
\alias{colMaxs}
\alias{colMaxs,hyperSpec-method}
\alias{colMeans}
\alias{colMeans,hyperSpec-method}
\alias{colMedians}
\alias{colMedians,hyperSpec-method}
\alias{colMins}
\alias{colMins,hyperSpec-method}
\alias{colOrderStats}
\alias{colOrderStats,hyperSpec-method}
\alias{colProds}
\alias{colProds,hyperSpec-method}
\alias{colQuantiles}
\alias{colQuantiles,hyperSpec-method}
\alias{colRanges}
\alias{colRanges,hyperSpec-method}
\alias{colRanks}
\alias{colRanks,hyperSpec-method}
\alias{colSds}
\alias{colSds,hyperSpec-method}
\alias{colSums}
\alias{colSums,hyperSpec-method}
\alias{colTabulates}
\alias{colTabulates,hyperSpec-method}
\alias{colVars}
\alias{colVars,hyperSpec-method}
\alias{colWeightedMeans.hyperSpec}
\alias{colWeightedMedians.hyperSpec}
\alias{matrixStats}
\alias{rowAlls}
\alias{rowAlls,hyperSpec-method}
\alias{rowAnys}
\alias{rowAnys,hyperSpec-method}
\alias{rowCollapse}
\alias{rowCollapse,hyperSpec-method}
\alias{rowCounts}
\alias{rowCounts,hyperSpec-method}
\alias{rowDiffs}
\alias{rowDiffs,hyperSpec-method}
\alias{rowIQRs}
\alias{rowIQRs,hyperSpec-method}
\alias{rowMads}
\alias{rowMads,hyperSpec-method}
\alias{rowMaxs}
\alias{rowMaxs,hyperSpec-method}
\alias{rowMeans}
\alias{rowMeans,hyperSpec-method}
\alias{rowMedians}
\alias{rowMedians,hyperSpec-method}
\alias{rowMins}
\alias{rowMins,hyperSpec-method}
\alias{rowOrderStats}
\alias{rowOrderStats,hyperSpec-method}
\alias{rowProds}
\alias{rowProds,hyperSpec-method}
\alias{rowQuantiles}
\alias{rowQuantiles,hyperSpec-method}
\alias{rowRanges}
\alias{rowRanges,hyperSpec-method}
\alias{rowRanks}
\alias{rowRanks,hyperSpec-method}
\alias{rowSds}
\alias{rowSds,hyperSpec-method}
\alias{rowSums}
\alias{rowSums,hyperSpec-method}
\alias{rowVars}
\alias{rowVars,hyperSpec-method}
\alias{rowWeightedMeans.hyperSpec}
\alias{rowWeightedMedians.hyperSpec}
\title{matrixStats functions for hyperSpec objects}
\usage{
  \S4method{anyMissing}{hyperSpec}(x, ...)

  \S4method{colAlls}{hyperSpec}(x, na.rm = TRUE, ...)

  \S4method{colAnys}{hyperSpec}(x, na.rm = TRUE, ...)

  \S4method{rowAlls}{hyperSpec}(x, na.rm = TRUE, ...)

  \S4method{rowAnys}{hyperSpec}(x, na.rm = TRUE, ...)

  \S4method{colMeans}{hyperSpec}(x, ..., label.spc,
    user = NULL, short = "colMeans", date = NULL)

  \S4method{colSums}{hyperSpec}(x, ..., label.spc,
    user = NULL, short = "colSums", date = NULL)

  \S4method{colCollapse}{hyperSpec}(x, ..., label.spc,
    user = NULL, short = "colCollapse", date = NULL)

  \S4method{colCounts}{hyperSpec}(x, ..., label.spc,
    user = NULL, short = "colCounts", date = NULL)

  \S4method{colDiffs}{hyperSpec}(x, ..., label.spc,
    user = NULL, short = "colDiffs", date = NULL)

  \S4method{colIQRs}{hyperSpec}(x, ..., label.spc,
    user = NULL, short = "colIQRs", date = NULL)

  \S4method{colMads}{hyperSpec}(x, ..., label.spc,
    user = NULL, short = "colMads", date = NULL)

  \S4method{colMaxs}{hyperSpec}(x, ..., label.spc,
    user = NULL, short = "colMaxs", date = NULL)

  \S4method{colMedians}{hyperSpec}(x, ..., label.spc,
    user = NULL, short = "colMedians", date = NULL)

  \S4method{colMins}{hyperSpec}(x, ..., label.spc,
    user = NULL, short = "colMins", date = NULL)

  \S4method{colOrderStats}{hyperSpec}(x, ..., label.spc,
    user = NULL, short = "colOrderStats", date = NULL)

  \S4method{colProds}{hyperSpec}(x, ..., label.spc,
    user = NULL, short = "colProds", date = NULL)

  \S4method{colQuantiles}{hyperSpec}(x, ..., label.spc,
    user = NULL, short = "colQuantiles", date = NULL)

  \S4method{colRanges}{hyperSpec}(x, ..., label.spc,
    user = NULL, short = "colRanges", date = NULL)

  \S4method{colRanks}{hyperSpec}(x, ..., label.spc,
    user = NULL, short = "colRanks", date = NULL)

  \S4method{colSds}{hyperSpec}(x, ..., label.spc,
    user = NULL, short = "colSds", date = NULL)

  \S4method{colTabulates}{hyperSpec}(x, ..., label.spc,
    user = NULL, short = "colTabulates", date = NULL)

  \S4method{colVars}{hyperSpec}(x, ..., label.spc,
    user = NULL, short = "colVars", date = NULL)

  \method{colWeightedMeans}{hyperSpec} (x, ..., label.spc,
    user = NULL, short = "colWeightedMeans", date = NULL)

  \method{colWeightedMedians}{hyperSpec} (x, ...,
    label.spc, user = NULL, short = "colWeightedMedians",
    date = NULL)

  \S4method{rowMeans}{hyperSpec}(x, ..., label.wavelength,
    user = NULL, short = "rowMeans", date = NULL)

  \S4method{rowSums}{hyperSpec}(x, ..., label.wavelength,
    user = NULL, short = "rowSums", date = NULL)

  \S4method{rowCollapse}{hyperSpec}(x, ...,
    label.wavelength, user = NULL, short = "rowCollapse",
    date = NULL)

  \S4method{rowCounts}{hyperSpec}(x, ..., label.wavelength,
    user = NULL, short = "rowCounts", date = NULL)

  \S4method{rowDiffs}{hyperSpec}(x, ..., label.wavelength,
    user = NULL, short = "rowDiffs", date = NULL)

  \S4method{rowIQRs}{hyperSpec}(x, ..., label.wavelength,
    user = NULL, short = "rowIQRs", date = NULL)

  \S4method{rowMads}{hyperSpec}(x, ..., label.wavelength,
    user = NULL, short = "rowMads", date = NULL)

  \S4method{rowMaxs}{hyperSpec}(x, ..., label.wavelength,
    user = NULL, short = "rowMaxs", date = NULL)

  \S4method{rowMedians}{hyperSpec}(x, ...,
    label.wavelength, user = NULL, short = "rowMedians",
    date = NULL)

  \S4method{rowMins}{hyperSpec}(x, ..., label.wavelength,
    user = NULL, short = "rowMins", date = NULL)

  \S4method{rowOrderStats}{hyperSpec}(x, ...,
    label.wavelength, user = NULL, short = "rowOrderStats",
    date = NULL)

  \S4method{rowProds}{hyperSpec}(x, ..., label.wavelength,
    user = NULL, short = "rowProds", date = NULL)

  \S4method{rowQuantiles}{hyperSpec}(x, ...,
    label.wavelength, user = NULL, short = "rowQuantiles",
    date = NULL, drop = FALSE)

  \S4method{rowRanges}{hyperSpec}(x, ..., label.wavelength,
    user = NULL, short = "rowRanges", date = NULL)

  \S4method{rowRanks}{hyperSpec}(x, ..., label.wavelength,
    user = NULL, short = "rowRanks", date = NULL)

  \S4method{rowSds}{hyperSpec}(x, ..., label.wavelength,
    user = NULL, short = "rowSds", date = NULL)

  \S4method{rowVars}{hyperSpec}(x, ..., label.wavelength,
    user = NULL, short = "rowVars", date = NULL)

  \method{rowWeightedMeans}{hyperSpec} (x, ...,
    label.wavelength, user = NULL,
    short = "rowWeightedMeans", date = NULL)

  \method{rowWeightedMedians}{hyperSpec} (x, ...,
    label.wavelength, user = NULL,
    short = "rowWeightedMedians", date = NULL)
}
\arguments{
  \item{x}{hyperSpec object}

  \item{label.spc}{labels for the intensity axis for
  loadings-like statistics}

  \item{label.wavelength}{labels for the wavelength axis
  for scores-like statistics}

  \item{user,short,date}{handed to
  \code{\link[hyperSpec]{logentry}}}

  \item{drop,na.rm,...}{further parameters to the
  \link[matrixStats]{matrixStats-package} function}
}
\description{
  hyperSpec objects can use matrix functions from package
  \link[matrixStats]{matrixStats-package} in addition to
  the base functions \code{\link[base]{colMeans}},
  \code{\link[base]{colSums}}, \code{\link[base]{rowMeans}}
  and \code{\link[base]{rowSums}}.
}

