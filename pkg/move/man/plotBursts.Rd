\name{plotBursts}

\alias{plotBursts}
\alias{plotBursts,list-method}
\alias{plotBursts,.MoveTrackSingleBurst-method}

\docType{methods}

\title{Plotting the centroids of a track}

\description{Plotting bursted track information}

\usage{
  \S4method{plotBursts}{list}(object, add=TRUE, sizeFUN=function(x) {as.numeric(diff(range(timestamps(x))), units ="mins")}, col = NA, breaks = 3, ...)
  \S4method{plotBursts}{.MoveTrackSingleBurst}(object, add=TRUE, sizeFUN=function(x) {as.numeric(diff(range(timestamps(x))), units ="mins")}, col = NA, breaks = 3, ...)
}

\arguments{
  \item{object}{a SpatialPointsDataFrame or a list of these that include coordinates and color, and size of the centroid indicators}
  \item{add}{logical, if FALSE a new plot is generated, default is TRUE}
  \item{sizeFUN}{a funciton to calculate the size of the plotted circles (see details)}
  \item{breaks}{how many size classes should the circles have, default is 3}
  \item{col}{a vector of color codes with the same length as the burstID. By default the standard colors from 1:8 are used. If there are more than 8 burstIDs the colors are recycled}
  \item{...}{additional plot attributes}
}

\details{
The plotBursts function plots bursted Move objects. Use the \code{burst} function to create a bursted Move Object. Per burst a circle is plotted at the center of the bursted segement. The colors correspond to the burstID and the size is defined by a function which is by default the relative duration of the segment compared to the whole duration. 
}

\author{Marco Smolla}

\examples{
test <- move(system.file("extdata","leroy.csv", package="move"))
behav <- c(rep(1:91,each=10), rep(92, 8))
testb <- burst(test, f=behav)
plot(coordinates(test),type="l")
plotBursts(testb, breaks=3, add=TRUE, pch=19)
plotBursts(testb, breaks=5, add=FALSE, pch=19)

#bursting track by realtive segment length
plotBursts(object=testb, breaks=3, sizeFUN=function(x) sum(seglength(x)), pch=19, add=FALSE)

}