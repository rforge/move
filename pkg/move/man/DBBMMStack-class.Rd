\name{DBBMMStack-class}

\alias{DBBMMStack}
\alias{DBBMMStack-class}

\docType{class}

\title{
  The DBBMMStack class
}

\description{
  The DBBMMStack object is created within the brownian.bridge.dyn function from a Move object. It includes beneath others a raster object and probabilities.
}

\section{Slots}{
  \describe{
    \item{DBMvar}{Object of class \code{"dBMvariance"}: includes the break.list and points of interest, ...; }
    \item{crs}{part of the \link{Raster-class}}
    \item{ext}{the extension factor set by the user}
    \item{extent}{part of the \link{Raster-class}}
    \item{filename}{part of the \link{Raster-class}}
    \item{layers}{part of the \link{Raster-class}}    
    \item{layernames}{part of the \link{Raster-class}}
    \item{method}{the method that was used to calculate the utilization distribution, e.g. dynamic Brwonian Bridge}
    \item{ncols}{part of the \link{Raster-class}}
    \item{nrows}{part of the \link{Raster-class}}
    \item{rotated}{part of the \link{Raster-class}}
    \item{rotation}part of the \link{Raster-class}}
    \item{title}{part of the \link{Raster-class}}
    \item{z}{part of the \link{Raster-class}}
  }
}

\section{Methods}{
  \describe{
    \item{\code{\link{contour}}}{\code{signature(object = "DBBMMStack")}: adds a contour line to a plot}
    \item{\code{\link{image}}}{\code{signature(object = "DBBMMStack")}: plots the raster from a DBBMMStack object with fixed cell size ratio}
    \item{\code{\link{plot}}}{\code{signature(object = "DBBMMStack")}: plots the raster from a DBBMMStack object with resize insenstive proportions}
    \item{\code{\link{proj4string}}}{\code{signature(object = "DBBMMStack")}: extracts the projection method of the raster stored within the DBBMMStack object}
    \item{\code{\link{raster}}}{\code{signature(object = "DBBMMStack")}: extracts the raster from the DBBMMStack object}
    \item{\code{\link{outerProbability}}}{\code{signature(object = "DBBMMStack")}: calculates the animal occurence probabilities at the border of the raster}
  }
}

\author{Marco Smolla}

\keyword{classes}