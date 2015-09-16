\name{move2ade}

\alias{move2ade}
\alias{move2ade,.MoveTrackSingle-method}
\alias{move2ade,.MoveTrackStack-method}

\docType{methods}

\title{Convert a Move or MoveStack object to adehabitat compatible object}

\description{Convert a Move or MoveStack object to adehabitat compatible object. This is necessary because Move and MoveStack objects are not inherited by the object class that is typically used by the adehabitat package. Therefore, the \code{move2ade} function allows to use functions of the adehabitatHR package with objects that were originally created with the Move package. 
}

\usage{
  \S4method{move2ade}{.MoveTrackSingle}(x)
  \S4method{move2ade}{.MoveTrackStack}(x)
}

\arguments{
  \item{x}{a Move or MoveStack object}
}

\value{
  The returned object is from SpatialPointsDataFrame with the animal name (or 'unnamed') stored in the data slot of the SpatialPointsDataFrame.
}

\author{Marco Smolla}

\examples{
  data(fishers)
  data(leroy)
  move2ade(leroy) #for a Move object
  move2ade(fishers) #for a MoveStack object
}
