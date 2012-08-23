\name{move2ade}

\alias{move2ade}
\alias{move2ade,.MoveTrackSingle-method}
\alias{move2ade,.MoveTrack-method}

\docType{methods}

\title{Convert a Move or MoveStack object to adehabitat compatible object}

\description{Convert a Move or MoveStack object to adehabitat compatible object}

\usage{
  \S4method{move2ade}{.MoveTrackSingle}(x)
  \S4method{move2ade}{.MoveTrack}(x)
}

\arguments{
  \item{x}{a Move or MoveStack object}
}

\details{
   Convert a Move or MoveStack object to an object that can be used with the adeHabitat package.
}

\value{
  The returned object is from SpatialPointsDataFrame with the animal name (or 'unnamed') stored in the data slot of the SpatialPointsDataFrame.
}

\author{Marco Smolla}

\examples{
  load(system.file("extdata", "leroy.RData", package="move"), .GlobalEnv)
  move2ade(data) #for a Move object
  move2ade(data2) #for a MoveStack object
}
