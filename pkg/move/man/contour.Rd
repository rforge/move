\name{contour}

\alias{contour}
\alias{contour,DBBMM-method}

\docType{methods}

\title{Contour plot}

\description{Contour plot of a RasterLayer from a DBBMM object (see \code{\link{DBBMM-class}}).}

\usage{
  \S4method{contour}{DBBMM}(x,levels,add,...)
}

\arguments{
  \item{x}{a DBBMM object}
  \item{levels}{numeric, between 0 and 1; describes the levels to be displayed}
  \item{add}{logical, if TRUE the contour is added to a plot}
  \item{...}{additional arguments}
}

\details{
  The contour function creates a shape of the area in which there is a 90\% probability that the animal will be located. One or several probabilities can be set with \code{levels}. At least one level must be added, there is no default value set.
}

\author{Marco Smolla}

\examples{
\dontrun{
## create a move object
data <- move(system.file("data","leroy.csv",package="move"),proj=CRS("+proj=longlat"))

## transform to aequd projection and center the map
data2 <- spTransform(data, center=TRUE)

## create a DBBMM object
p <- brownian.bridge.dyn(object=data2, location.error=23.5, dimSize=45, ext=.2)

## to add a 50\% and 90\% contour to a plot from DBBMM object p
plot(p)
contour(p, levels=c(.5,.9), add=TRUE)
}
}