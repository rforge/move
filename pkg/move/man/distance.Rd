\name{distance}

\alias{distance}
\alias{distanceSummary}
\alias{distance,.MoveTrackSingle-method}
\alias{distance,.MoveTrackStack-method}
\alias{distanceSummary,.MoveTrackSingle-method}
\alias{distanceSummary,.MoveTrackStack-method}

\docType{methods}

\title{distance information from a track or track stack}

\description{This function returns a summary about distance related measurments of a track or track stack}

\usage{
  \S4method{distance}{.MoveTrackSingle}(x)
  \S4method{distance}{.MoveTrackStack}(x)
  \S4method{distanceSummary}{.MoveTrackSingle}(x)
  \S4method{distanceSummary}{.MoveTrackStack}(x)
}

\arguments{
  \item{x}{Move or MoveStack object}
}

\value{
All values are returned in kilometer if the projection of the coordinates is longlat.
}

\author{Marco Smolla}

\examples{
  load(system.file("extdata", "move.RData", package="move"), .GlobalEnv)
  distance(leroy) #distances from a Move object 
  distance(stack) #distances from a MoveStack object
  distanceSummary(leroy) # summary of distance measures of a Move object
  distanceSummary(stack) # summary of distance measures of a MoveStack object
}