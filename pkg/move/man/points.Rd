\name{points}

\alias{points}
\alias{points,Move-method}

\docType{methods}

\title{Plotting a track as points}

\description{Function for plotting a recorded track from a Move object as points}

\usage{
  \S4method{points}{Move}(x,add=FALSE,...)
}

\arguments{
  \item{x}{Move or a DBBMM object}
  \item{add}{if TRUE the points are added to a plot}
  \item{...}{arguments to be passed on, e.g. \code{col} for color, or \code{add} to add the points to a plot.}
}

\author{Marco Smolla}