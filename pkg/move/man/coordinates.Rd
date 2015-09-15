\name{coordinates}

\alias{coordinates}
\alias{coordinates,Move-method}

\docType{methods}

\title{Extract the track coordinates from a Move/MoveStack object}

\description{The coordinates method extracts the coordinates of a track.}

\usage{
  \S4method{coordinates}{Move}(obj,...)
}

\arguments{
  \item{obj}{A valid Move or MoveStack object}
  \item{...}{Additional arguments, see Details}
}

\details{
Returns a matrix with the coordinates of the track in a Move or MoveStack object. 
}

\examples{
## create a move object
data(leroy)
## extract the coordinates
coords <- coordinates(leroy)
\dontshow{
data(stack)
coordinates(stack)
}}

\author{Marco Smolla}
