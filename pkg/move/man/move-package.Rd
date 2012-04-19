\name{move-package}

\alias{move-package}

\docType{package}

\title{
An overview of the functions in this package
}

\description{
move is a package that contains functions to access movement data stored in \url{www.movebank.org} as well as tools to visualize and statistically analyse animal movement data. Move addresses movement ecological questions.
}

\details{
 The package implements classes for movement data and supports
  \itemize{
    \item Creation of Move objects (see Move-class) representing animals and their track
    \item Calculation of utilization densities using the dynamic Brownian briedge Movement Model
    \item Plotting tracks, utilization densities and contours
    \item Access to raster, n.col, projection and coordinates
    \item Different CRS projection methods like longlat or aequd
  }
}

\section{I. Creating Move objects}{

Move objects can be created from files with the function:
  \tabular{ll}{
    \code{move}\tab To create a Move object \cr
  }
}

\section{II. Calculation of the utilization density}{

With the function below the dynamic Brownian Bridge Movement Model calculates the utilitzation density from a Move object:
  \tabular{ll}{
    \code{brownian.bridge.dyn}\tab To calculate the utilization density\cr
  }
}

\section{III. Accessing values}{

  \tabular{ll}{
    \code{coordinates}\tab Track-coordinates of the Move Object\cr
    \code{as.data.frame}\tab A data.frame with the important data of the Move Object\cr
    \code{n.locs}\tab The number of locations\cr
    \code{time.lag}\tab The time lags between the locations\cr
    \code{projection}\tab The projection method of the track/raster\cr
    }
}

\section{IV. Plotting data}{

The track or the utilizazion density can be plotted with the following functions
  \tabular{ll}{
    \code{plot}\tab To plot the utilization density with fixed width and hight ratio (see DBBMM-class)\cr or the track (see Move-class) \cr
    \code{image}\tab To plot the utilization density fitted to the window \cr
    \code{contour}\tab Adds the contours of utilization densities to a plot\cr 
  }
}

\author{
Bart Kranstauber, Marco Smolla, Kamran Safi \cr \cr
Maintainer: Bart Kranstauber \email{bkranstauber@orn.mpg.de}, Marco Smolla \email{msmolla@orn.mpg.de}
}

\references{
CRAN: \url{http://move.r-forge.r-project.org/}
}

\keyword{package}