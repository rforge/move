\name{spTransform}

\alias{spTransform}
\alias{spTransform,Move,character-method}
\alias{spTransform,Move,missing-method}

\docType{methods}

\title{Transform Move object projection method}

\description{The spTransform function transforms the coordinates stored in the Move object from the default longlat coordinates to the default aeqd (Azimuthal Equi-distance) projection or a different projection.}

\usage{
  \S4method{spTransform}{Move,character}(x,CRSobj,center=FALSE)
  \S4method{spTransform}{Move,missing}(x,center=FALSE,...)
}

\arguments{
  \item{x}{a Move or a MoveStack object}
  \item{CRSobj}{a CRS like character that describes the projection method to which the coordinates should be transformed, if missing \code{"+proj=aequd"} is used as default value}
  \item{center}{logical, if TRUE the center of the coordinate system is the center of the track; FALSE is default}
  \item{...}{for additional arguments}
}

\details{
  The spTransform function transforms the coordinates of a Move object by default from \code{"+proj=longlat"} to \code{"+proj=aeqd"}. In this format the coordinates can be used by the \code{\link{brownian.bridge.dyn}} function. \cr
  If \code{center} is TRUE the center of the coordinate system is set to the center of the track.
}

\examples{
## create a Move object
leroy <- move(system.file("extdata","leroy.csv.gz",package="move"))

## transform the Move object by default into "+aeqd" projection method 
## and center the coordinate system
spTransform(leroy, center=TRUE)

## transform the Move object into another projection method, like mollweide
spTransform(leroy, CRSobj="+proj=moll")

##check projection method
proj4string(leroy)
\dontshow{
spTransform(leroy, CRSobj=CRS("+proj=moll"))
load(system.file("extdata", "move.RData", package="move"), .GlobalEnv)
spTransform(stack)
}
}

\author{Marco Smolla}
