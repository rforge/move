\name{lines}

\alias{lines}
\alias{lines,.MoveTrackSingle-method}
\alias{lines,.MoveTrackStack-method}
\alias{lines,.MoveTrackSingleBurst-method}

\docType{methods}

\title{Plotting a track as lines}

\description{Function for plotting a recorded track from a Move object as lines}

\usage{
  \S4method{lines}{.MoveTrackSingle}(x,...)
  \S4method{lines}{.MoveTrackStack}(x,col=NA,...)
  \S4method{lines}{.MoveTrackSingleBurst}(x,col=NA,...)
}

\arguments{
  \item{x}{Move object}
  \item{col}{a vector of colors (same length as the number of individual for a stack or number of burst levels or segments for a burst object, or one)}
  \item{...}{arguments to be passed on, e.g. \code{lty} or \code{lwd}.}
}


\author{Marco Smolla}

\seealso{
	\code{\link{points}}
	\code{\link{plot}}
}

\examples{
data(leroy)
data(leroydbbmm)
plot(leroydbbmm)
lines(spTransform(leroy, center=TRUE), col=3) # add a track from a Move object to a plot
\dontshow{
data(fishers)
plot(fishers, type='p')
lines(fishers, col=3:4, pch=4) %just test whether MoveStacks can be plotted
}
}
