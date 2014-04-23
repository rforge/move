\name{dynBGB}
\alias{dynBGB}
\alias{dynBGB,.MoveTrackSingle,RasterLayer,numeric-method}
\alias{dynBGB,.MoveTrackSingle,numeric,ANY-method}
\alias{dynBGB,dBGBvariance,RasterLayer,numeric-method}
\alias{dynBGB,.MoveTrackSingle,ANY,character-method}
\alias{dynBGB,.MoveTrackSingle,missing,ANY-method}

\docType{methods}

\title{Calculation of the dynamic Bivariate Gausian Bridge}
\arguments{
  \item{move}{the move object or variance object used for
  calculating the ud if a \code{.MoveTrackSingle} object is
  supplied this is converted into a \code{dBGBvariance}
  object using the \code{dynBGBvariance} function}

  \item{raster}{either the raster used for UD
  calculation or the resolution of the raster used for UD calculation}

  \item{locErr}{the location errors used for the
  calculation}
}
\description{
	This function creates a utilization distribution according to the Bivariate Gaussian Bridge model. It returns an object of the class \link{dynBGB-class}.
}
\author{Bart Kranstauber}
\references{
	Kranstauber, B., Safi, K., Bartumeus, F.. (2014), Bivariate Gaussian bridges: directional factorization of diffusion in Brownian bridge models. Movement Ecology 2:5. doi:10.1186/2051-3933-2-5.
}
\examples{
data <- move(system.file("extdata","leroy.csv.gz",package="move")) [230:280,]

## change projection method to aeqd and center the coordinate system to the track
dataAeqd <- spTransform(data, CRSobj="+proj=aeqd", center=TRUE)

dBGB <- dynBGB(dataAeqd, locErr=9, raster=10, ext=2.15, windowSize=31, timeStep=6, margin=15)
plot(dBGB, col=hsv(sqrt(1:700/1000)))
lines(dataAeqd)
}


