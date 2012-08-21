\name{seglength}

\alias{seglength}
\alias{seglength,SpatialPointsDataFrame-method}

\docType{methods}

\title{Calculates the segment length of a track}

\description{Calculates the segment length of a track}

\usage{
  \S4method{seglength}{SpatialPointsDataFrame}(x)
}

\arguments{
  \item{x}{a SpatialPointsDataFrame, like a Move or MoveStack object}
}

\details{
  The seglength function calculates the distances between point 1 and point 2, point 2 and point 3, ... .  
}

\value{
  If the SptialPointsDataFrame is projected in longitude latitude coordiantes, the returned values are in kilometer. If not the distnces are calculated with the Pythagoras' theorem.
}

\author{Marco Smolla}

\examples{
  load(system.file("extdata", "leroy.RData", package="move"), .GlobalEnv)
  head(seglength(data)) #Move object in longlat projection
  head(seglength(testtest2)) #MoveStack object in aeqd projection
}