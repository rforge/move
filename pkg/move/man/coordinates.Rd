\name{coordinates}

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
  \code{obj} needs to be a Move object. The object includes a special data frame with the track coordinates and a time stamp. 
}

\author{Marco Smolla}