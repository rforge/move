\name{MoveStack-class}

\alias{MoveStack}
\alias{MoveStack-class}
\alias{[,MoveStack,ANY,ANY-class}

\docType{class}

\title{
  The MoveStack class
}

\description{
  The MoveStack object is created within the brownian.bridge.dyn function from a Move object. It includes among others a raster object and probabilities.
}

\section{Slots}{
  \describe{
    \item{citation}{Object of class \code{"dBMvariance"}: includes the break.list and points of interest, ...; }
    \item{bbox}{belongs to the SpatialPointsDataFrame}
    \item{coords}{the extension factor set by the user}
    \item{coords.nrs}{belongs to the SpatialPointsDataFrame}
    \item{data}{additional data of that object that is stored in the SpatialPointsDataFrame}
    \item{dateCreation}{date and timestamp when this object was created}
    \item{idData}{additional data to all individuals}
    \item{license}{the license terms of the used track material}
    \item{proj4string}{projection of all coordinates}
    \item{study}{name of the study}
    \item{timestamps}{timestamps according to the coordinates}
    \item{trackId}{a vector that indicates, which data, coordinates and timestamps belong to an individual}
  }
}

\section{Methods}{
  Methods defined with class "MoveStack" in the signature:
    \describe{
      \item{\link[=subset]{[}}{\code{signature(x="MoveStack")}: select subset ...}
      \item{\code{\link{getMovebankData}}}{\code{signature(object = "character")}: creates a MoveStack object by accessing Movebank}
      }
}

\author{Marco Smolla}

\keyword{classes}