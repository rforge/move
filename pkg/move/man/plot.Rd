\name{plot}

\alias{plot}
\alias{plot,.MoveTrackSingle,missing-method}
\alias{plot,.MoveTrackStack,missing-method}
\alias{plot,.MoveTrackSingleBurst,missing-method}

\docType{methods}

\title{Plotting track or raster}

\description{Function for plotting a recorded track from a Move object or the probability values from a DBBMM object}

\usage{
  \S4method{plot}{.MoveTrackSingle,missing}(x, y,...)
  \S4method{plot}{.MoveTrackStack,missing}(x, y, type="p", ...)
  \S4method{plot}{.MoveTrackSingleBurst,missing}(x, y, type="p", ...)
}

\arguments{
  \item{x}{Move, MoveStack, MoveBurst or DBBMM object}
  \item{y}{unused variable (listed for compatibility reasons)}
  \item{type}{defines the type of the plot (e.g. 'l', 'p', 'b', 'o')}
  %\item{col}{specifies color. For Movestacks default is NA (R colors are used). It can also be a vector wich is as long as the number of tracks or coordinates}
  \item{...}{arguments to be passed to methods, such as graphical parameters, and the logical add argument (see \code{\link{par}})}
}

\details{
  If \code{x} is a Move, MoveStack object a track is plotted with points and lines. The track can be added to another plot with the \code{add = TRUE}. \cr
  If \code{x} is a MoveBurst object colored lines (according to the burstID) are plotted if the type is set to 'l'. By default it is 'p' which plots the real coordinates of the Move object as points.
  If \code{x} is a DBBMM object its raster object is plotted with the corresponding cell values. Unlike the \code{\link{image}} function, the cell size ratio keeps the same when the plot window is re-sized.
}

\note{
Have a look on the proportion of the graphic device when printing a track or raster. The plot function does not use equal sized units on both axes.
}

\author{Marco Smolla}


\examples{
load(system.file("extdata", "move.RData", package="move"), .GlobalEnv)
stack <- moveStack(list(leroy,leroy))

plot(leroy) # plot a Move object
plot(leroy, type="o", col=3)
plot(stack, col=c(3,5), lwd=3) # plot a MoveStack object
plot(stack, type="l", col=c(3,5), lwd=3)

plot(leroydbbmm) # plot the raster of a DBBMM object
plot(dbbmmstack) # plot the raster of a DBBMMStack object

\dontshow{
example(burst)
plot(testb)
}
}
