\name{interpolateTime}

\alias{interpolateTime}
\alias{interpolateTime,.MoveTrackSingle,numeric-method}
\alias{interpolateTime,.MoveTrackSingle,POSIXct-method}
\alias{interpolateTime,.MoveTrackSingle,difftime-method}
\docType{methods}
\title{Interpolate a trajectory}
\description{This function allows to interpolate trajectories. It does this on the basis of a simple interpolation, depending on the \code{spaceMethod} that is specified.}
\usage{
	interpolateTime(x, time, spaceMethod=c('euclidean','greatcircle','rhumbline'),...)
}
\arguments{
  \item{x}{an object of the \link{Move-class}}
  \item{time}{either timestamps, or a number of locations or time interval that is used to generate a sequence of timestamps using the \code{seq} function}
  \item{spaceMethod}{a character that indicates the interpolation function to be used}
  \item{...}{other arguments currently not used}
}
\author{Bart Kranstauber}
\examples{
  load(system.file("extdata", "move.RData", package="move"), .GlobalEnv)
	plot(interpolateTime(ricky[1:200,], 700,'gr'))
}
