\name{move2ade}

\alias{move2ade}
\alias{move2ade,.MoveTrackSingle-method}

\docType{methods}

\title{Convert a Move object to adehabitat compatible object}

\description{Convert a Move object to adehabitat compatible object}

\usage{
  \S4method{move2ade}{.MoveTrackSingle}(x)
}

\arguments{
  \item{x}{a Move object}
}

\details{
   Convert a Move object to adehabitat compatible object
}

\value{
  The returned object is from SpatialPointsDataFrame with the animal name (or 'unnamed') stored in the data slot of the SpatialPointsDataFrame.
}

\author{Marco Smolla}

\examples{
  load(system.file("extdata", "leroy.RData", package="move"), .GlobalEnv)
  move2ade(data)
}
