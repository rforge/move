\name{contour}

\alias{contour}
\alias{contour,DBBMM-method}

\docType{methods}

\title{Contour plot}

\description{Contour plot of a RasterLayer from a DBBMM object (see \code{\link{DBBMM-class}}).}

\usage{
  \S4method{contour}{DBBMM}(x, y, add=F, plot=F, google=F, track=F, col="blue", lcol="brown", lwd=2, llwd=2, ...)
}

\arguments{
  \item{x}{a DBBMM object}
  \item{y}{corresponding Move object if you want to plot the track on a google map}
  \item{plot}{logical, if FALSE, the contour is returned as a SpatialLinesDataFrame. If TRUE, the contour is plotted. Default is FALSE}
  \item{add}{logical, if TRUE the contour is added to a plot, default is FALSE}
  \item{google}{logical, if TRUE plot is ommitted and the contour is plotted on top of a google map}
  \item{track}{logical, if track and google is TRUE, a track is plotted on top of the contour google map plot}
  \item{col}{color of the contour line}
  \item{lcol}{color of the track}
  \item{lwd}{line width of the contour line}
  \item{llwd}{line width of the track}
  \item{...}{additional arguments, see details}
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
cnt  <- contour(dbbmm, levels=c(.4,.9))

##print the contour on a google map +add the track
require("RgoogleMaps")
cnt  <- contour(x=dbbmm, y=data, levels=c(.4,.9), google=TRUE)
cnt  <- contour(x=dbbmm, y=data, levels=c(.4,.9), google=TRUE, track=TRUE)
}
}