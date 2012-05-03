\name{brownian.bridge.dyn}

\alias{brownian.bridge.dyn}
\alias{brownian.bridge.dyn,Move,RasterLayer,missing,numeric-method}
\alias{brownian.bridge.dyn,Move,numeric,missing,numeric-method}
\alias{brownian.bridge.dyn,Move,missing,missing,numeric-method}
\alias{brownian.bridge.dyn,Move,missing,numeric,numeric-method}

\docType{methods}

\title{Creates a DBBMM object}

\description{The brownian.bridge.dyn function uses a Move object (see \code{\link{Move-class}}) to calculate the utilization density (UD) of the given track. It uses the dynamic Brownian Bridge Movement Model (dBBMM) to do so. The dBBMM has the advantage over the other Brownian Bridge Movement Model that changes in behaviour are accounted for. It does so by using the behavioral change point analysis in a sliding window. For details see Kranstauber et al. (2012).}

\usage{
  brownian.bridge.dyn(object, raster, dimSize, location.error,
                 margin=11, time.step=NULL, window.size=31, ext, ...)
}


\arguments{
  \item{object}{a Move object, which can be created with the function \code{move}}
  \item{raster}{can be a RasterLayer object or numeric. If it is a RasterObject and dimSize is not set, \code{brownian.bridge.dyn} starts to calculate the UD. If \code{raster} is numeric, it describes the cell.size of the raster. Default is 1}
  \item{dimSize}{can be numeric. If there is no input for raster, dimSize describes the number of cells of the largest x or y dimenstion. Default is 10}
  \item{location.error}{required numeric value. Describes the error of the location (sender/receiver) system in map units.}
  \item{margin}{The margin used for the behavioral change point analysis.}
  \item{time.step}{The size of the time steps taken for the calculation of the utilization density in minutes, if left null 15 steps are taken in the shortest time interval.}
  \item{window.size}{The size of the moving window along the track. Larger windows provide more stable/accurate estimates of the brownian motion variance but are less well able to capture more frequent changes in behaviour.}
  \item{ext}{numeric value >0. Decribes the amount of extension of the bounding box of the animal track. It can be one (same extension into all four directions), two (first x, then y directional extension) or four numbers (xmin, xmax, ymin, ymax extension). Default is .25 (extends the bounding box by 25\%)}
  \item{...}{for additional arguments}
}

\details{
  There are four ways to launch the brownian.bridge.dyn function which are as follows:\cr \cr
  1. Serve a raster \cr
  A RasterLayer object is set for the raster argument which is then used to calculate the UD. \cr \cr
  2. Set the cell size\cr
  To set the cell size set a numeric value for the raster argument but skip the dimSize. The numeric raster argument is used as the cell sizes of the raster. A RasterLayer object is created and returned to the same function.\cr \cr
  3. Set the number of cells (col/row) \cr
  To set the number of cells along the largest dimension a numeric dimSize argument is set instead of the raster argument. A RasterLayer object is created and returned to the same function.\cr \cr
  4. Using default raster \cr
  When there are no values set, the default raster value is used to calculate and create a RasterLayer object, which is returned to the same function. Note: depending on the size of the area of interest, the default cell size value can result in a large number of cells which may take a very long time to calculate!

  The function prints an estimate of the size of the computational task ahead. This can give an indication of how long the computation is going to take. It should scale roughtly linearly with the duration of the computations. In our experience $1*10^9$ takes about a minute with a normal laptop. 
}  

\note{
	Note that the first few and last few segments of the trajectory are omitted in the calculation of the UD since a lower number of estimates for the Brownian motion variance are obtained for those segments.
}

\author{Marco Smolla}
\references{
	Kranstauber, B., Kays, R., LaPoint, S. D., Wikelski, M. and Safi, K. (2012), A dynamic Brownian bridge movement model to estimate utilization distributions for heterogeneous animal movement. Journal of Animal Ecology. doi: 10.1111/j.1365-2656.2012.01955.x
}

\examples{
## create a move object
data <- move(system.file("extdata","leroy.csv",package="move"),proj=CRS("+proj=longlat"))

## change projection method to aequd and center the coordinate system to the track
data2 <- spTransform(data, CRSobj="+proj=aeqd", center=TRUE)

## create a DBBMM object
p <- brownian.bridge.dyn(object=data2, location.error=23.5, dimSize=45, ext=.3)
}
