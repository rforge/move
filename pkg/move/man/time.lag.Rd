\name{time.lag}

\alias{time.lag}
\alias{time.lag,.MoveTrackSingle-method}
\alias{time.lag,MoveStack-method}

\docType{methods}

\title{Calculates the time lags between the coordinates}

\description{The time.lag function calculates the time lags between locations.}

\usage{
  \S4method{time.lag}{.MoveTrackSingle}(x,...)
  \S4method{time.lag}{MoveStack}(x,...)
}

\arguments{
  \item{x}{a Move or MoveStack object}
  \item{...}{further arguments}
}

\details{Optionally the argument units can be passed on to ensure the time lag in a certain unit. For more information on the units argument see the help of difftime.}

\value{
  The function returns a numeric vector with the time lags.
}

\author{Marco Smolla}

\examples{
load(system.file("extdata", "leroy.RData", package="move"), .GlobalEnv)
time.lag(data) #calculate timelags for a Move object
time.lag(data2) #calculate timelags for a MoveStack object
}