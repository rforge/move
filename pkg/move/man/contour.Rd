\name{contour}

\alias{contour}
\alias{contour,.UD-method}

\docType{methods}

\title{Contour plot}

\description{Contour plot of a RasterLayer from a DBBMM object.}

\usage{
  \S4method{contour}{.UD}(x, ...)
}

\arguments{
  \item{x}{an object of the \link{DBBMM-class}}
  \item{...}{additional arguments like levels and nlevels, see details}
}

\details{
The contour function creates a shape of the area in which there is a 90\% probability that the animal will be located. One or several probabilities can be set with \code{levels}. Choose values between 0 and 1. At least one level must be added, there is no default value set. You can also use \code{nlevel} to set a number of fixed distance levels. \cr
To change parameters of the contour or line plotting use the usual parameters of the \code{plot} function.
}

\author{Marco Smolla}

\examples{
\dontrun{
## Create a DBBMM object to use this function
## See the examples of the brownian.bridge.dyn function
## Here we just load a previous session, where we already created a DBBMM object
load(system.file("extdata", "leroy.RData", package="move"), .GlobalEnv)

## to add a 50\% and 90\% contour to a plot from DBBMM object dbbmm
plot(dbbmm)
contour(dbbmm, levels=c(.5,.9), add=TRUE, plot=TRUE)

##to store the contours as a SpatialLinesDataFrame use
cnt  <- contour(dbbmm, levels=c(.4,.9), plot=FALSE)

##print the contour on a google map +add the track
require("RgoogleMaps")
cnt  <- contour(x=dbbmm, y=data, levels=c(.4,.9), google=TRUE)
cnt  <- contour(x=dbbmm, y=data, levels=c(.4,.9), google=TRUE, track=TRUE)
}
\dontshow{
contour(dbbmmStack, levels=c(.9,.5), col=c(2,5))
}
}