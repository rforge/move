\name{distance}

\alias{distance}
\alias{distanceSummary}
\alias{distance,.MoveTrackSingle-method}
\alias{distance,.MoveTrackStack-method}
\alias{distanceSummary,.MoveTrackSingle-method}
\alias{distanceSummary,.MoveTrackStack-method}

\docType{methods}

\title{distance information from a track or track stack}

\description{This function returns a summary of distance related measurements of a track or track stack}

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
All values are returned in kilometers if the projection of the coordinates is longlat. Check and set the projection of your Move or MoveStack object using the proj4string() function.
}

\author{Marco Smolla}

\examples{
  load(system.file("extdata", "move.RData", package="move"), .GlobalEnv)
  stack <- moveStack(list(leroy,leroy))
  
  distance(leroy) #distances from a Move object 
  distance(stack) #distances from a MoveStack object
  distanceSummary(leroy) # summary of distance measures of a Move object
  distanceSummary(stack) # summary of distance measures of a MoveStack object
}