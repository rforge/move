\name{lines}

\alias{lines}
\alias{lines,.MoveTrackSingle-method}
\alias{lines,.MoveTrackStack-method}

\docType{methods}

\title{Plotting a track as lines}

\description{Function for plotting a recorded track from a Move object as lines}

\usage{
  \S4method{lines}{.MoveTrackSingle}(x,...)
  \S4method{lines}{.MoveTrackStack}(x,col=NA,...)
}

\arguments{
  \item{x}{Move object}
  \item{col}{a vector of colors (same length as the number of objects)}
  \item{...}{arguments to be passed on, e.g. \code{col} for color, or \code{add} to add the lines to a plot.}
}


\author{Marco Smolla}

\examples{
load(system.file("extdata", "move.RData", package="move"), .GlobalEnv)

plot(rickydbbmm)
lines(spTransform(ricky), col=3) # add a track from a Move object to a plot
\dontshow{
lines(stack, col=3:4, pch=4) %just test whether MoveStacks can be plotted
}
}