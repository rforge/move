\name{n.locs}

\alias{n.locs}
\alias{n.locs,SpatialPointsDataFrame-method}

\docType{methods}

\title{Extract the number of locations of a Move or MoveStack object}

\description{The n.locs method returns the number of locations of a track from a Move or MovesStack object.}

\usage{
  \S4method{n.locs}{SpatialPointsDataFrame}(obj)
}

\arguments{
  \item{obj}{Move or MoveStack object}
}

\author{Marco Smolla}

\examples{
load(system.file("extdata", "leroy.RData", package="move"), .GlobalEnv)

n.locs(data) #number of locations of Move object
n.locs(data2) #number of locations of MoveStack object
}