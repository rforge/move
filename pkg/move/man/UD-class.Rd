\name{.UD-class}

\alias{.UD}
\alias{.UD-class}
\alias{.UDStack-class}

\docType{class}

\title{
The UD class
}

\description{
	This Class represents a simple abstraction of the utilization distribution, UD, where all probabilities necessarily sum to one. It is exported for experienced user to program against.
}

\section{Slots}{
  \describe{
    \item{crs}{part of the \link{Raster-class}}
%    \item{ext}{the extension factor set by the user}
    \item{data}{part of the \link{Raster-class}}
    \item{extent}{part of the \link{Raster-class}}
    \item{file}{part of the \link{Raster-class}}
    \item{history}{part of the \link{Raster-class}}
    \item{layernames}{part of the \link{Raster-class}}
    \item{legend}{part of the \link{Raster-class}}
    \item{method}{stores the method that was used to calculate the utilization distribution (UD), e.g. dynamic Brwonian Bridge}
    \item{ncols}{part of the \link{Raster-class}}
    \item{nrows}{part of the \link{Raster-class}}
    \item{rotated}{part of the \link{Raster-class}}
    \item{rotation}{part of the \link{Raster-class}}
    \item{title}{part of the \link{Raster-class}}
    \item{z}{part of the \link{Raster-class}}
  }
}

%\section{Methods}{
%  \describe{
%    \item{\code{\link{contour}}}{\code{signature(object = "DBBMM")}: adds a contour line to a plot}
%    \item{\code{\link{image}}}{\code{signature(object = "DBBMM")}: plots the raster from a DBBMM object with fixed cell size ratio}
%    \item{\code{\link{plot}}}{\code{signature(object = "DBBMM")}: plots the raster from a DBBMM object with resize insenstive proportions}
%    \item{\code{\link{proj4string}}}{\code{signature(object = "DBBMM")}: extracts the projection method of the raster stored within the DBBMM object}
%    \item{\code{\link{raster}}}{\code{signature(object = "DBBMM")}: extracts the raster from the DBBMM object}
%    \item{\code{\link{outerProbability}}}{\code{signature(object = "DBBMM")}: calculates the animal occurence probabilities at the border of the raster}
%  }
%}

\author{Bart Kranstauber}

\keyword{classes}
