\name{hrBootstrap}

\alias{hrBootstrap}
\alias{hrBootstrap,SpatialPoints-method}
\alias{hrBootstrap,.MoveTrackStack-method}

\docType{methods}

\title{Calculates and plot the Minimum Convex Polygon for a track}

\description{Calculates and plot the Minimum Convex Polygon for a track}

\usage{
  \S4method{hrBootstrap}{SpatialPoints}(x, rep=100, plot=TRUE, level=95, levelMax=100, unin='km', unout='m2', ...)
  \S4method{hrBootstrap}{.MoveTrackStack}(x, rep=100, plot=TRUE, level=95, levelMax=100, unin="km", unout="m2", ...)

}

\arguments{
  \item{x}{a Move or MoveStack object, or a simple SpatialPoints object}
  \item{rep}{numeric value for the number of repetitions per sample size, default is 100}
  \item{plot}{logical value that indicates whether the graph is plotted or not, default is TRUE}
  \item{level}{the percentage of coordinates taken into account for the MCP area size calculation, default is 95 (95\% of all coordinates are taken into account)}
  \item{levelMax}{the percentage of coordinates taken into account for the maximum MCP area size calculation (horizontal line in the plot)}
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
  hrBootstrap(m,rep=11, level=99, unout="m2", plot=TRUE) #for a Move object
  hrBootstrap(moveStack(list(m,m)),rep=11, unout="m2", plot=FALSE) #for a MoveStack object
  
  hrBootstrap(as(m,"SpatialPoints"),rep=25, unout="m2", plot=TRUE) #for a SpatialPoints object
}

