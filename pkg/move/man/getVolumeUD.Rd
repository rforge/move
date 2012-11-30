\name{getVolumeUD}

\alias{getVolumeUD}
\alias{getVolumeUD,Raster-method}

\docType{methods}

\title{getVolumeUD of a raster}

\description{Make raster cells correlating to their contour}

\usage{
  \S4method{getVolumeUD}{Raster}(x, ...)
}

\arguments{
  \item{x}{an object of the \link{DBBMM-class} or \link{Raster-class} }
  \item{...}{additional Raster or DBBMM objects}
}

\details{
The getVolumeUD returns a new Raster where the cells have the value (between 0 and 1) to its corresponding contour. }

\author{Marco Smolla}

\examples{
load(system.file("extdata", "move.RData", package="move"), .GlobalEnv)

getVolumeUD(leroydbbmm) #for a single object
getVolumeUD(leroydbbmm, leroydbbmm, leroydbbmm) #for several objects 
}
