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
The contour function creates a shape of the area in which the animal can be found by a certain probability (i.e. the 90\% contour describes the area in which the animal can be found with the 90\% probability). One or several probabilities can be set with \code{levels} (numeric or vector of values between 0 and 1). If no value is set all contour lines are returned. You can also use \code{nlevel} to set a number of fixed distance levels. \cr
To change parameters of the contour or line plotting use the usual parameters of the \code{plot} function (like lwd, lty, and so on). 
You can also add the contour lines to a plot by adding \code{add = TRUE}.
}

\author{Marco Smolla}

\examples{
data(leroydbbmm)
## to add a 50\% and 95\% contour to a plot from DBBMM object dbbmm
plot(leroydbbmm)
contour(leroydbbmm, levels=c(.5,.95), add=TRUE)
}
