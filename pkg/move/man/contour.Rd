\name{contour}

\alias{contour}
\alias{contour,.UD-method}
\alias{contour,.UDStack-method}

\docType{methods}

\title{Contour plot}

\description{Contour plot of a RasterLayer from a DBBMM object.}

\usage{
  \S4method{contour}{.UD}(x, ...)
  \S4method{contour}{.UDStack}(x, ...)
}

\arguments{
  \item{x}{an object of the \link{DBBMM-class} or \link{DBBMMStack-class} }
  \item{...}{additional arguments like levels and nlevels, see details}
}

\details{
The contour function creates a shape of the area in which the animal were for a certain amount of time (e.g. 90\% of the time). One or several probabilities can be set with \code{levels}. Choose values between 0 and 1. At least one level must be added, there is no default value set. You can also use \code{nlevel} to set a number of fixed distance levels. \cr
To change parameters of the contour or line plotting use the usual parameters of the \code{plot} function. 
You can also add the contour lines to a plot with \code{add = TRUE}.
}

\author{Marco Smolla}

\examples{
load(system.file("extdata", "move.RData", package="move"), .GlobalEnv)

## to add a 50\% and 90\% contour to a plot from DBBMM object dbbmm
plot(leroydbbmm)
contour(leroydbbmm, levels=c(.5,.9), add=TRUE)

##to store the contours as a SpatialLinesDataFrame use
#cnt  <- contour(dbbmm, levels=c(.4,.9), add=FALSE)

##print the contour on a google map +add the track
#cnt  <- contour(x=dbbmm, y=data, levels=c(.4,.9), google=TRUE)
#cnt  <- contour(x=dbbmm, y=data, levels=c(.4,.9), google=TRUE, track=TRUE)
}
