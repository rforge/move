\name{brownian.motion.variance.dyn}

\alias{brownian.motion.variance.dyn}
\alias{brownian.motion.variance.dyn,.MoveTrackSingle,numeric,numeric,numeric-method}
\alias{brownian.motion.variance.dyn,.MoveTrackSingleBurst,numeric,numeric,numeric-method}

\title{Calculates the dynamic brownian motion variance}

\description{
	A function to calculate the dynamic brownian motion variance for a movement track. It can be used by advanced programmers to program against.
}

\usage{
\S4method{brownian.motion.variance.dyn}{.MoveTrackSingle,numeric,numeric,numeric}(object, location.error, window.size, margin)
}

\arguments{
	\item{object}{An object of the \link{Move-class}, that can be used for variance calculation. It needs to be in a flat coordinate system.}
	\item{location.error}{A numeric vector with the location error.}
	\item{window.size}{The window size used for the variance calculation. }
	\item{margin}{The margin size used for variance calculation.}
}

\value{
	An object of the type dBMvariance is returned
}

\references{
	Kranstauber, B., Kays, R., LaPoint, S. D., Wikelski, M. and Safi, K. (2012), A dynamic Brownian bridge movement model to estimate utilization distributions for heterogeneous animal movement. Journal of Animal Ecology. doi: 10.1111/j.1365-2656.2012.01955.x
}

\author{Bart Kranstauber}


\seealso{
	\code{\link{brownian.bridge.dyn}}
}

\examples{
data(leroy)
data2 <- spTransform(leroy[1:80,], CRSobj="+proj=aeqd +ellps=WGS84", center=TRUE)
err<-rep(23.5,n.locs(data2))
dBMvar <- brownian.motion.variance.dyn(data2, location.error=err, margin=13, window.siz=31)
dBMvar
}
