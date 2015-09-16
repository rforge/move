\name{equalProj}

\alias{equalProj}
\alias{equalProj,list-method}

\docType{methods}

\title{Checks projections for being equal}

\description{Checks whether all objects of a list are in the same projection}

\usage{
  \S4method{equalProj}{list}(x)
}

\arguments{
  \item{x}{a list of projected objects, like DBBMM or Raster objects, returning the projection string with with the function proj4string }
}

\details{
  equalProj checks for equal projections using the function of identicalCRS from the package sp. It returns true if none of the objects have a proj4 string.
}

\author{Bart Kranstauber}

\examples{
data(ricky)
data(leroy)
data(leroydbbmm)
equalProj(list(leroydbbmm,leroydbbmm))
  equalProj(list(leroy,leroydbbmm))
  equalProj(list(leroy,ricky))
}
