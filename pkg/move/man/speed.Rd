\name{speed}

\alias{speed}
\alias{speedSummary}
\alias{speed,.MoveTrackSingle-method}
\alias{speed,.MoveTrackStack-method}
\alias{speedSummary,.MoveTrackSingle-method}
\alias{speedSummary,.MoveTrackStack-method}

\docType{methods}

\title{speed information from a track or track stack}

\description{This function returns a summary about speed related measurments of a track or track stack}

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
The returned value is by default meters per second.
}

\examples{
  load(system.file("extdata", "move.RData", package="move"), .GlobalEnv)  
  speed(ricky) #speeds from a Move object 
  speed(stack) #speeds from a MoveStack object
  speedSummary(leroy) # summary of speed measures of a Move object
  speedSummary(stack) # summary of speed measures of a MoveStack object
}