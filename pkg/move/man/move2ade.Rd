\name{move2ade}

\alias{move2ade}
\alias{move2ade,.MoveTrackSingle-method}
\alias{move2ade,.MoveTrack-method}

\docType{methods}

\title{Convert a Move or MoveStack object to adehabitat compatible object}

\description{Convert a Move or MoveStack object to adehabitat compatible object. This is necessary because Move and MoveStack objects are not inherited by the object class that is typically used by the adehabitat package. Therefore, the \code{move2ade} function allows to use functions of the adehabitatHR package with objects that were originally created with the Move package. 
}

\usage{
  \S4method{move2ade}{.MoveTrackSingle}(x)
  \S4method{move2ade}{.MoveTrack}(x)
}

\arguments{
  \item{x}{a Move or MoveStack object}
}

\value{
  The returned object is from SpatialPointsDataFrame with the animal name (or 'unnamed') stored in the data slot of the SpatialPointsDataFrame.
}

\author{Marco Smolla}

\examples{
  load(system.file("extdata", "move.RData", package="move"), .GlobalEnv)
  move2ade(ricky) #for a Move object
  move2ade(stack) #for a MoveStack object
}
