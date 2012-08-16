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
  load(system.file("extdata", "leroy.RData", package="move"), .GlobalEnv)
  
  time(data) # summary of time measures of a Move object
  time(data2) # summary of time measures of a MoveStack object
  
  time.lag(data)
  time.lag(testtest2)
}