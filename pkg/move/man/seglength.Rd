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
  If the SpatialPointsDataFrame is projected in longitude latitude coordinates, the returned values are in meters. If not, the distances are Euclidean distances in map units. Distances are calculates with the \code{pointDistance} from the package raster. Note that in stacks distances are not split between animals.
}

\author{Marco Smolla}

\examples{
data(leroy)
data(fishers)  
head(seglength(leroy)) #Move object in longlat projection
head(seglength(fishers)) #MoveStack object in aeqd projection
}
