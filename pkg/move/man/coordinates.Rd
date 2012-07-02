\name{coordinates}

\alias{coordinates}
\alias{coordinates,Move-method}

\docType{methods}

\title{Extract the track coordinates from a Move object}

\description{The coordinates method extracts the coordinates of the track within a Move object.}

\usage{
  \S4method{coordinates}{Move}(obj,...)
}

\arguments{
  \item{obj}{A valid Move object}
  \item{...}{Additional arguments, see Details}
}

\details{
  \code{obj} The move object includes a special data frame with the track coordinates and a time stamp. They are returned as a matrix.
}

\examples{
## create a move object
data <- move(system.file("extdata","leroy.csv",package="move"), proj=CRS("+proj=longlat"))

## extract the coordinates
coords <- coordinates(data)
}

\author{Marco Smolla}