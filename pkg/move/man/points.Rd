\name{points}

\alias{points}
\alias{points,.MoveTrackSingle-method}
\alias{points,.MoveTrackStack-method}
\alias{points,.MoveTrackSingleBurst-method}

\docType{methods}

\title{Plotting the points of a track}

\description{Function for plotting a recorded track from a Move object as points.}

\usage{
  \S4method{points}{.MoveTrackSingle}(x,...)
  \S4method{points}{.MoveTrackStack}(x,col=NA,...)
  \S4method{points}{.MoveTrackSingleBurst}(x,...)
}

\arguments{
  \item{x}{Move or a DBBMM object}
  \item{col}{a vector of colors (same length as the number of objects)}
  \item{...}{arguments to be passed on, e.g. \code{col} for color, or \code{add} to add the points to a plot.}
}

\author{Marco Smolla}

\examples{
load(system.file("extdata", "move.RData", package="move"), .GlobalEnv)

plot(rickydbbmm)
points(spTransform(ricky), col=3) # add a track from a Move object to a plot
\dontshow{
stack <- moveStack(list(leroy,leroy))
points(stack, col=3:4, pch=4) #just test whether MoveStacks can be plotted
}
}