\name{time}

\alias{time}
\alias{time,.MoveTrackSingle-method}
\alias{time,.MoveTrackStack-method}

\docType{methods}

\title{time information from a track or track stack}

\description{This function returns a summary about time related measurments of a track or track stack}

\usage{
  \S4method{time}{.MoveTrackSingle}(x)
  \S4method{time}{.MoveTrackStack}(x)
}

\arguments{
  \item{x}{Move or MoveStack object}
}

\author{Marco Smolla}

\examples{
  load(system.file("extdata", "move.RData", package="move"), .GlobalEnv)
  
  time(ricky) # summary of time measures of a Move object
  time(stack) # summary of time measures of a MoveStack object
  
  time.lag(leroy)
  time.lag(stack)
}