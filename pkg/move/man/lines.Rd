\name{lines}

\alias{lines}
\alias{lines,Move-method}

\docType{methods}

\title{Plotting a track as lines}

\description{Function for plotting a recorded track from a Move object as lines}

\usage{
  \S4method{lines}{Move}(x,add=FALSE,...)
}

\arguments{
  \item{x}{Move or a DBBMM object}
  \item{add}{if TRUE the lines are added to a plot}
  \item{...}{arguments to be passed on, e.g. \code{col} for color, or \code{add} to add the lines to a plot.}
}

\author{Marco Smolla}