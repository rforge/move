\name{timeSummary}

\alias{timeSummary}
\alias{timeSummary,.MoveTrackSingle-method}
\alias{timeSummary,.MoveTrackStack-method}

\docType{methods}

\title{time information from a track or track stack}

\description{This function returns a summary about time related measurements of a Move or MoveStack object.}

\usage{
  \S4method{timeSummary}{.MoveTrackSingle}(x, units="hours")
  \S4method{timeSummary}{.MoveTrackStack}(x, units="hours")
}

\arguments{
  \item{x}{Move or MoveStack object}
  \item{units}{defines the output uni, 'secs', 'mins', 'hours', 'days', ...}
}

\author{Marco Smolla}

\examples{
  data(fishers)
  data(leroy)
  timeLag(leroy)
  timeLag(fishers)
  timeSummary(leroy) # summary of time measures of a Move object
  timeSummary(fishers, units="hours") # summary of time measures of a MoveStack object
}
