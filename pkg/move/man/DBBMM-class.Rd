\name{DBBMM-class}

\alias{DBBMM}
\alias{DBBMM-class}

\docType{class}

\title{
The DBBMM class
}

\description{
The DBBMM object is created within the brownian.bridge.dyn function from a Move object. It includes beneath others a raster object and probabilities.
}

\section{Slots}{
  \describe{
    \item{DBMvar}{Object of class \code{"DBMvar"}: includes the break.list and points of interest, ...; }
    \item{raster}{Object of class \code{"character"}: the raster object with probability values  }
  }
}

\section{Methods}{
  \describe{
    \item{\code{\link{contour}}}{\code{signature(object = "DBBMM")}: adds a contour line to a plot}
    \item{\code{\link{image}}}{\code{signature(object = "DBBMM")}: plots the raster from a DBBMM object with fixed cell size ratio}
    \item{\code{\link{plot}}}{\code{signature(object = "DBBMM")}: plots the raster from a DBBMM object with resize insenstive proportions}
    \item{\code{\link{proj4string}}}{\code{signature(object = "DBBMM")}: extracts the projection method of the raster stored within the DBBMM object}
    \item{\code{\link{raster}}}{\code{signature(object = "DBBMM")}: extracts the raster from the DBBMM object}
    \item{\code{\link{outerProbability}}}{\code{signature(object = "DBBMM")}: calculates the animal occurence probabilities at the border of the raster}
  }
}

\author{Marco Smolla}

\keyword{classes}