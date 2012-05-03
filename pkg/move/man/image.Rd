\name{image}

\alias{image}
\alias{image,DBBMM-method}

\docType{methods}

\title{Display a color image of DBBMM raster}

\description{Creates a grid of colored or gray-scale rectangles with colors corresponding to the values stored in the raster object of the DBBMM object.}

\usage{
  \S4method{image}{DBBMM}(x,col=rainbow(356),...)
}

\arguments{
  \item{x}{a DBBMM object}
  \item{col}{a vector of continous colors, see \code{\link{grDevices}}}
  \item{...}{additional arguments}
}

\details{
 Unlike than the \code{\link{plot}} function of a DBBMM object, the image function does not keep the ratio of cell dimensions of the raster after resizing the plot window. Yet, if a track is added with \code{plot(x=Move, add=TRUE)} the spatial coherence remains after resizing the plotting window.
}

\author{Marco Smolla}

\note{
  Originally based on a function by Thomas Lumley.
}

\examples{
## create a move object, transform it, and create a DBBMM object
data <- move(system.file("extdata","leroy.csv",package="move"),proj=CRS("+proj=longlat"))
data2 <- spTransform(data, center=TRUE)
p <- brownian.bridge.dyn(object=data2, location.error=23.5, dimSize=45, ext=.2)

## create a grid of colored rectangles of the raster
image(p)
}