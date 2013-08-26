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
  equalProj checks for equal projections using the function of identicalCRS from the package sp. 
}

\author{Bart Kranstauber}

\examples{
  load(system.file("extdata", "move.RData", package="move"), .GlobalEnv)
  equalProj(list(leroydbbmm,leroydbbmm))
  equalProj(list(leroy,leroydbbmm))
  equalProj(list(leroy,ricky))
}
