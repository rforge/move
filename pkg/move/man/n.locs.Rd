\name{n.locs}

\alias{n.locs}
\alias{n.locs,Move-method}

\docType{methods}

\title{Extract the number of locations of a track from a Move object}

\description{The n.locs method extracts the number of locations of track from a Move object.}

\usage{
  \S4method{n.locs}{Move}(obj)
}

\arguments{
  \item{obj}{A Move object}
}

\details{
  \code{obj} needs to be a Move object. The object includes a special data frame with the track coordinates and a time stamp. 
}

\author{Marco Smolla}