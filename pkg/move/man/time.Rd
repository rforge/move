\name{timeSummary}

\alias{timeSummary}
\alias{timeSummary,.MoveTrackSingle-method}
\alias{timeSummary,.MoveTrackStack-method}

\docType{methods}

\title{time information from a track or track stack}

\description{This function returns a summary about time related measurments of a track or track stack}

\usage{
  \S4method{timeSummary}{.MoveTrackSingle}(x)
  \S4method{timeSummary}{.MoveTrackStack}(x)
}

\arguments{
  \item{x}{Move or MoveStack object}
}

\author{Marco Smolla}

\examples{
  load(system.file("extdata", "move.RData", package="move"), .GlobalEnv)
  time.lag(leroy)
  time.lag(stack)
  timeSummary(ricky) # summary of time measures of a Move object
  timeSummary(stack) # summary of time measures of a MoveStack object
}