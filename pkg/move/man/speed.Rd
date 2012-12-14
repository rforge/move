\name{speed}

\alias{speed}
\alias{speedSummary}
\alias{speed,.MoveTrackSingle-method}
\alias{speed,.MoveTrackStack-method}
\alias{speedSummary,.MoveTrackSingle-method}
\alias{speedSummary,.MoveTrackStack-method}

\docType{methods}

\title{speed information from a track or track stack}

\description{This function returns a summary of speed related measurements of a Move or MoveStack object.}

\usage{
  \S4method{speed}{.MoveTrackSingle}(x)
  \S4method{speed}{.MoveTrackStack}(x)
  \S4method{speedSummary}{.MoveTrackSingle}(x)
  \S4method{speedSummary}{.MoveTrackStack}(x)
}

\arguments{
  \item{x}{Move or MoveStack object}
}

\author{Marco Smolla}

\value{
  For objects that are longlat projected (see ?proj4string) the returned values are in meters per second.
}

\examples{
  load(system.file("extdata", "move.RData", package="move"), .GlobalEnv)  
  stack <- moveStack(list(leroy,leroy))
  
  speed(ricky) #speeds from a Move object 
  speed(stack) #speeds from a MoveStack object
  speedSummary(leroy) # summary of speed measures of a Move object
  speedSummary(stack) # summary of speed measures of a MoveStack object
}