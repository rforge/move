\name{brownian.bridge.dyn}

\alias{brownian.bridge.dyn}
\alias{brownian.bridge.dyn,.MoveTrackSingle,RasterLayer,missing,numeric-method}
\alias{brownian.bridge.dyn,MoveStack,RasterLayer,missing,numeric-method}
\alias{brownian.bridge.dyn,.MoveTrackSingle,missing,missing,numeric-method}
\alias{brownian.bridge.dyn,SpatialPointsDataFrame,missing,numeric,ANY-method}
\alias{brownian.bridge.dyn,SpatialPointsDataFrame,numeric,missing,ANY-method}
%\alias{brownian.bridge.dyn,Move,numeric,missing,numeric-method}
%\alias{brownian.bridge.dyn,Move,missing,numeric,numeric-method}
\alias{brownian.bridge.dyn,dBMvariance,RasterLayer,missing,numeric-method}
\alias{brownian.bridge.dyn,dBMvarianceBurst,RasterLayer,missing,numeric-method}
\alias{brownian.bridge.dyn,.MoveTrackSingle,RasterLayer,missing,character-method}

\docType{methods}

\title{Creates a DBBMM object}

\description{The brownian.bridge.dyn function uses a Move object (see \code{\link{Move-class}}) to calculate the utilization distribution, UD, of the given track. It uses the dynamic Brownian Bridge Movement Model (dBBMM) to do so. The dBBMM has the advantage over the other Brownian Bridge Movement Model that changes in behavior are accounted for. It does so by using the behavioral change point analysis in a sliding window. For details see references.}

\usage{
  brownian.bridge.dyn(object, raster, dimSize, location.error,
                 margin=11, window.size=31, ext=.3, bbox=NA,...)
}


\arguments{
  \item{object}{an object of the \link{Move-class}}
  \item{raster}{a RasterLayer object or numeric value. A numeric value for \code{raster} is interpreted as the edge size of the square raster cells (in map units); the according raster will be calculated internally. If a RasterLayer is provided the \code{brownian.bridge.dyn} starts to calculate the UD based on that raster.}
  \item{dimSize}{numeric. dimSize is only used if \code{raster} is not set. \code{dimSize} is interpreted as the number of cells along the largest dimension of the track. The according raster will be calculated internally. Default is 10}
  \item{location.error}{single numeric value or vector of the length of coordinates that describes the error of the location (sender/receiver) system in map units, or a character string with the name of the column containing the location error.}
  \item{margin}{The margin used for the behavioral change point analysis.}
  \item{window.size}{The size of the moving window along the track. Larger windows provide more stable/accurate estimates of the brownian motion variance but are less well able to capture more frequent changes in behavior.}
  \item{ext}{Describes the amount of extension of the bounding box around the animal track. It can be numeric (same extension into all four directions), vector of two (first x, then y directional extension) or vector of four  (xmin, xmax, ymin, ymax extension). Default is .25 (extends the bounding box by 25\%). Only considered in combination with a numeric raster argument or the dimSize argument. }
  \item{bbox}{vector with 4 numbers defining a bounding box for the raster}
  \item{...}{for additional arguments to program against}
}

\details{
  There are four ways to launch the brownian.bridge.dyn function which are as follows:\cr \cr
  1. Use a raster \cr
  A RasterLayer object is set for the raster argument which is then used to calculate the UD. \cr \cr
  2. Set the cell size\cr
  To set the cell size, set a numeric value for the raster argument without providing dimSize. The numeric raster argument is used as the cell sizes of the raster. \cr \cr
  3. Set the number of cells (col/row) \cr
  To set the number of cells along the largest dimension a numeric dimSize argument can be set. \cr \cr
  4. Using default raster \cr
  When there are no values set, the default raster value is used to calculate and create a RasterLayer object, which is returned to the same function. Note: depending on the size of the area of interest, the default cell size value can result in a large number of cells which may take a very long time to calculate!

  The function prints an estimate of the size of the computational task ahead. This can give an indication of how long the computation is going to take. It should scale roughly linearly with the duration of the computations. In our experience 10e9 takes about a minute with an average laptop. 

  There is one further argument that can be given: \code{time.step}. It correspond to the size of the timer intervals taken for every integration step (in minutes). If left \code{NULL} 15 steps are taken in the shortest time interval.
}  

\note{
	Note that the first few and last few segments of the trajectory are omitted in the calculation of the UD since a lower number of estimates for the Brownian motion variance are obtained for those segments.

	Thanks to Ryan Nielson for making the BBMM package that served as an example for early versions of this code.
}

\author{Bart Kranstauber, Marco Smolla}
\references{
	Kranstauber, B., Kays, R., LaPoint, S. D., Wikelski, M. and Safi, K. (2012), A dynamic Brownian bridge movement model to estimate utilization distributions for heterogeneous animal movement. Journal of Animal Ecology. doi: 10.1111/j.1365-2656.2012.01955.x
}

\examples{
## create a move object
data <- move(system.file("extdata","leroy.csv.gz",package="move"))[1:90,]

## change projection method to aeqd and center the coordinate system to the track
data2 <- spTransform(data, CRSobj="+proj=aeqd", center=TRUE)

## create a DBBMM object
dbbmm <- brownian.bridge.dyn(object=data2, location.error=12, dimSize=45, ext=.3, 
			     time.step=600, margin=15)
\dontshow{
  load(system.file("extdata", "move.RData", package="move"), .GlobalEnv)
  leroy <- leroy[1:50]
  ricky <- ricky[1:50]
  stack <- moveStack(list(leroy,ricky))
  t <- brownian.bridge.dyn(spTransform(stack, center=TRUE), location.error=23.5, dimSize=25, 
			   time.step=6000, ext=1.2)
}
}
