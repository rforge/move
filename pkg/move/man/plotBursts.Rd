\name{plotBursts}

\alias{plotBursts}
\alias{plotBursts,list-method}
\alias{plotBursts,.MoveTrackSingleBurst-method}

\docType{methods}

\title{Plotting the centroids of a track}

\description{
  The \code{plotBursts} function plots bursted Move objects (see ?burst for how to create a bursted Move Object). For every single burst a circle is plotted at the center of every segment of a track. A segment represents consecutive coordinates that belong to a single burst. The properties of the plotted circles (size and color) can represent different information (see Details).
  }

\usage{
  \S4method{plotBursts}{list}(object, add=TRUE, 
			      sizeFUN=function(x) {as.numeric(diff(range(timestamps(x))), 
							      units ="mins")}, 
			      col = NA, breaks = 3, ...)
  \S4method{plotBursts}{.MoveTrackSingleBurst}(object, add=TRUE, 
					       sizeFUN=function(x) {
						       as.numeric(diff(range(timestamps(x))), 
									       units ="mins")}, 
					       col = NA, breaks = 3, ...)
}

\arguments{
  \item{object}{a SpatialPointsDataFrame or a list of these that include coordinates and color, and size of the centroid indicators}
  \item{add}{logical, if FALSE a new plot is generated, default is TRUE}
  \item{sizeFUN}{a function to calculate the size of the plotted circles (see details)}
  \item{breaks}{how many size classes should the circles have, default is 3}
  \item{col}{a vector of color codes with the same length as the burstID. By default the standard colors from 1:8 are used. If there are more than 8 burstIDs the colors are recycled}
  \item{...}{additional plot attributes}
}

\details{
Circle colors correspond to burstIDs. The size of the circles is defined by a function. By default this function calculates the relative duration of a segment compared to the whole duration. The function can be adjusted to use a different measure using the \code{sizeFUN} argument. \cr
Note, it is possible to define break points by using the \code{breaks} argument.
}

\author{Marco Smolla}

\examples{
test <- move(system.file("extdata","leroy.csv.gz", package="move"))
behav <- c(rep(1:4,each=200), rep(5, 118))
testb <- burst(test, f=behav)
plot(coordinates(test),type="l")
plotBursts(testb, breaks=3, add=TRUE, pch=19)
plotBursts(testb, breaks=5, add=FALSE, pch=19)

##plotBursts(leroy_b, breaks=c(-Inf,4000,6000, Inf), add=FALSE, pch=19) 

#bursting track by realtive segment length
plotBursts(object=testb, breaks=3, sizeFUN=function(x) sum(seglength(x)), pch=19, add=FALSE)

}
