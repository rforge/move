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
  \item{x}{a list of projected objects, like DBBMM or Raster objects }
}

\details{
  equalProj checks for equal projections. If an object has additional arguments (e.g. when it is centered) but the rest of its arguments are equal to the other objects the function returns TRUE with the warning that there are additional values. 
}

\author{Marco Smolla}

\examples{
  load(system.file("extdata", "move.RData", package="move"), .GlobalEnv)
  
  equalProj(list(leroydbbmm,leroydbbmm))
}
