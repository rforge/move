\name{points}

\alias{points}
\alias{points,.MoveTrackSingle-method}

\docType{methods}

\title{Plotting a track as points}

\description{Function for plotting a recorded track from a Move object as points}

\usage{
  \S4method{points}{.MoveTrackSingle}(x,add=TRUE,...)
}

\arguments{
  \item{x}{Move or a DBBMM object}
  \item{add}{if TRUE the points are added to a plot}
  \item{...}{arguments to be passed on, e.g. \code{col} for color, or \code{add} to add the points to a plot.}
}

\author{Marco Smolla}

\examples{
load(system.file("extdata", "leroy.RData", package="move"), .GlobalEnv)

points(data, col="yellow") # add a track from a Move object to a plot
#points(data2, col="3", pch=4, add=FALSE) # add a track from a MoveStack object to a plot
}