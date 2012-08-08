\name{coordinates}

\alias{coordinates}
\alias{coordinates,Move-method}

\docType{methods}

\title{Extract the track coordinates from a Move/MoveStack object}

\description{The coordinates method extracts the coordinates of the track in a Move or MoveStack object.}

\usage{
  \S4method{coordinates}{Move}(obj,...)
}

\arguments{
  \item{obj}{A valid Move or MoveStack object}
  \item{...}{Additional arguments, see Details}
}

\details{
  \code{obj} The move object includes a special data frame with the track coordinates and a time stamp. They are returned as a matrix.
}

\examples{
## create a move object
data <- move(system.file("extdata","leroy.csv",package="move"))

## extract the coordinates
coords <- coordinates(data)
\dontshow{
coordinates(moveStack(list(data,data)))
}}

\author{Marco Smolla}
