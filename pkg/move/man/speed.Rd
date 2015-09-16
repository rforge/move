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
	 The function returns the speed in m/s if the track is longlat else map units per second. In most cases the map units will also be meters. 
}

\examples{
data(leroy)
data(fishers)
  
  speed(leroy) #speeds from a Move object 
  speed(fishers) #speeds from a MoveStack object
  speedSummary(leroy) # summary of speed measures of a Move object
  speedSummary(fishers) # summary of speed measures of a MoveStack object
}
