\name{timeLag}

\alias{timeLag}
\alias{timeLag,.MoveTrackSingle-method}
\alias{timeLag,.MoveTrackStack-method}

\docType{methods}

\title{Calculates the time lags between the coordinates}

\description{The timeLag function calculates the time lags between locations.}

\usage{
  \S4method{timeLag}{.MoveTrackSingle}(x,...)
  \S4method{timeLag}{.MoveTrackStack}(x,units, ...)
}

\arguments{
  \item{x}{a Move or MoveStack object}
  \item{units}{The units used for the conversion, they need to be specified for a stack to ensure similair units between individuals}
  \item{...}{further arguments}
}

\details{Optionally the argument units can be passed on to ensure the time lag in a certain unit, this is especially useful in case a move stack is bursted. For more information on the units argument see the help of difftime.}

\value{
  The function returns a numeric vector with the time lags.
}

\author{Bart Kranstauber}

\examples{
load(system.file("extdata", "move.RData", package="move"), .GlobalEnv)
stack <- moveStack(list(leroy,ricky))

head(timeLag(leroy,units="mins")) #calculate timelags for a Move object
head(timeLag(stack,units="mins")) #calculate timelags for a MoveStack object
head(timeLag(stack, units="hours")) #calculate timelags in different units
}
