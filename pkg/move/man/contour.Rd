\name{contour}

\alias{contour,DBBMM-method}

\docType{methods}

\title{Contour plot}

\description{Contour plot of a RasterLayer from a DBBMM object.}

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
  The contour function allows to displays the area where e.g. the animal is found with a 90\% probability. One or several probabilities can be set with \code{levels}. At least one level must be added, there is no default set. 
}

\author{Marco Smolla}

\examples{
  
\dontrun{
#to add a 50\% and 90\% contour to a plot 
contour(, levels=c(.5,.9), add=TRUE)
}
}