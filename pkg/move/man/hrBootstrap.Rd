\name{hrBootstrap}

\alias{hrBootstrap}
\alias{hrBootstrap,.MoveTrackSingle-method}
\alias{hrBootstrap,.MoveTrack-method}

\docType{methods}

\title{Calculates and plot the Minimum Convex Polygon for a track}

\description{Calculates and plot the Minimum Convex Polygon for a track}

\usage{
  \S4method{hrBootstrap}{.MoveTrackSingle}(x, rep=100, plot=TRUE, unin='km', unout='m2', ...)
  \S4method{hrBootstrap}{.MoveTrack}(x, rep=100, plot=TRUE, unin="km", unout="m2", ...)

}

\arguments{
  \item{x}{a Move or MoveStack object}
  \item{rep}{numeric value for the number of repetitions per sample size, default is 100}
  \item{plot}{logical value that indicates whether the graph is plotted or not, default is TRUE}
  \item{unin}{units form the input values (can be 'm' or 'km')(as in adehabitat)}
  \item{unout}{units for the output values (can be 'm2', 'km2', or 'ha')(as in adehabitat)}
  \item{...}{a Move or MoveStack object}
}

\details{
  The hrBootstrap function calculates the 0, 25, 50, 75, 100\%  quantile of the Minimum Convex Polygon area by step wise (logarithmic) increasing the number of samples per calculation. For every step this calculation is repeated \code{rep} times with random coordinates from the track. For example it calculates 100 times the mcp area from 3 random locations and store the area. In the next step it calculates it from 5 random locations and so on. The returned graph shows the 5 quantlies of the area sizes. The dashed line indicates the real mcp area size of all locations.  
}

\note{
  Plots for MoveStacks are plotted one after another, and not side by side. 
}

\value{
  The returned values are reutnred in a data.frame with the units indicated by \code{unout}.
}
  
\author{Marco Smolla}
  
\examples{
  m <- move(x=rnorm(70), y=rnorm(70), time=as.POSIXct(1:70, origin="1970-1-1"), proj=CRS("+proj=aeqd"))    
  hrBootstrap(m,rep=25, unout="m2", plot=TRUE) #for a Move object
  hrBootstrap(moveStack(list(m,m)),rep=25, unout="m2", plot=FALSE) #for a MoveStack object
}

