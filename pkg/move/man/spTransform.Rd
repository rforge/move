\name{spTransform}

\alias{spTransform,Move,character-method}
\alias{spTransform,Move,missing-method}

\docType{methods}

\title{Transform Move object projection method}

\description{The spTransform function transforms the coordinates stored in the Move object from the defcault longlat coordinates to the default aequd projection method or a different projection.}

\usage{
  \S4method{spTransform}{Move,character}(x,CRSobj,center=FALSE)
  \S4method{spTransform}{Move,missing}(x,center=FALSE,...)
}

\arguments{
  \item{x}{a Move object}
  \item{CRSobj}{a CRS like chracter that describes the projection method to which the coordinates should be transformed, if missing "+proj=aequd" is used as default value}
  \item{center}{logical, if TRUE the coordinates center is the center of the track; FALSE as default}
  \item{...}{for additional arguments}
}

\details{
  The spTransform function transform the coordinates of a Move object by default from "+proj=longlat" to "+proj=aeqd". In this format the coordinates can be used by the \code{brownian.bridge.dyn} function. \cr
  If \code{center} is TRUE the center of the coordinate system is set to the center of the track.
}

\author{Marco Smolla}