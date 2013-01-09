\name{getVolumeUD}

\alias{getVolumeUD}
\alias{getVolumeUD,.UD-method}

\docType{methods}

\title{Modify a UD raster}

\description{Modifies the UD raster: that the cell values of the resulting raster are equal to the percentage of the smallest home range containing this cell.}

\usage{
  \S4method{getVolumeUD}{.UD}(x, ...)
}

\arguments{
  \item{x}{one or several objects of the \link{DBBMM-class} class}
  \item{...}{additional Raster or DBBMM objects}
}

%\details{
%The function \code{getVolumeUD} can be used to plot the 95\%contour of DBBMM object. Therefore, after a raster has been modified with the \code{getVolumeUD} function, all cells that are <= 0.95 are selected and plotted.  }

\author{Marco Smolla}

\examples{
load(system.file("extdata", "move.RData", package="move"), .GlobalEnv)

getVolumeUD(leroydbbmm) #for a single object
getVolumeUD(leroydbbmm, leroydbbmm, leroydbbmm) #for several objects 

plot(getVolumeUD(leroydbbmm))
}
