\name{burstTrack}

\alias{burstTrack}
\alias{burstTrack,.MoveTrackSingle,numeric-method}

\docType{methods}

\title{Bursting a track}

\description{Bursting a track by specified variable}

\usage{
  \S4method{burstTrack}{.MoveTrackSingle,numeric}(object, by, breaks=5, sizeFUN="relTime")
}

\arguments{
  \item{object}{a Move object}
  \item{by}{column indicator like \code{data$behavior} that is used to burst the track}
  \item{breaks}{defines the number of steps by which the relative values, e.g. relative time, is divided. Default value is 5.}
  \item{sizeFUN}{specify a function to calculate the sizes of the centroids like \code{function(ll) lapply(lapply(ll, length), FUN= function(x) (x/nrow(coordinates(object))))} DOES NOT WORK AT THE MOMENT! Default is relative time of the burst segemnt compared to the whole track}
}

\details{
The burstTrack function bursts (divides) a track in segments that are specified by the by attribute. If a colum with behavioral classfication is annotated to the coordinates of the Move object, the track can be bursted by that column. For every segment an indicator (a circle) is set in the middle of the segment. The size of this indicator depends on the sizeFUN and is by default defined by the relative time compared to the whole duration. 
}

\note{
  sizeFUN does not work at the moment
}

\author{Marco Smolla}

\examples{
test <- move(system.file("extdata","leroy.csv", package="move"))
test@data$behav <- c(rep(1:91,each=10), rep(92, 9))
testB1 <- burstTrack(object=test,by=test@data$behav)
plot(test)
plotBursts(testB1, pch=19)

testB2 <- burstTrack(object=test,by=test@data$behav, sizeFUN=function(ll, object) lapply(lapply(ll, length), FUN= function(x) (x/nrow(coordinates(object)))))
plot(test)
plotBursts(testB2, pch=19)

\dontshow{
testB3<- burstTrack(object=test,by=test@data$sensor)
plot(test)
plotBursts(testB3, pch=19)

t <- data.frame(A=c(1:7),B=c(1,1,1,1,2,3,4))
tM <- move(x=t$A,y=t$B,time=as.POSIXct(c(1:7), origin='1970-1-1'),proj=CRS('+proj=longlat'), animal=c(rep('anim',7)))
tM@data$behav <- c(rep(c(1,2),c(2,5)))
tMB <- burstTrack(tM,by=tM@data$behav)
plot(t)
plotBursts(tMB, pch=19) ##marco->wonder why the first point is larger than the second point 
}
}