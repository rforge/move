\name{lines}

\alias{lines}
\alias{lines,.MoveTrackSingle-method}

\docType{methods}

\title{Plotting a track as lines}

\description{Function for plotting a recorded track from a Move object as lines}

\usage{
  \S4method{lines}{.MoveTrackSingle}(x,add=TRUE,...)
}

\arguments{
  \item{x}{Move object}
  \item{add}{if TRUE the lines are added to a plot}
  \item{...}{arguments to be passed on, e.g. \code{col} for color, or \code{add} to add the lines to a plot.}
}


\author{Marco Smolla}


\examples{
load(system.file("extdata", "leroy.RData", package="move"), .GlobalEnv)

lines(data, col=7, add=FALSE) # add a track from a Move object to a plot
#lines(testtest2, col="blue", add=F) # add a track from a MoveStack object to a plot
}