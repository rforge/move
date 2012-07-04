\name{spTransform}

\alias{spTransform}
\alias{spTransform,Move,character-method}
\alias{spTransform,Move,missing-method}

\docType{methods}

\title{Transform Move object projection method}

\description{The spTransform function transforms the coordinates stored in the Move object from the default longlat coordinates to the default aequd projection method or a different projection.}

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
data <- move(system.file("extdata","leroy.csv",package="move"),proj=CRS("+proj=longlat"))

## transform the Move object by default into "+aeqd" projection method 
## and center the coordinate system
data2 <- spTransform(data, center=TRUE)

## transform the Move object into another projection method, like mollweide
data3 <- spTransform(data, CRSobj="+proj=moll")

##check projection method
proj4string(data)
"[1]  +proj=longlat +ellps=WGS84"
proj4string(data2)
"[1]   +proj=longlat +lon_0=-73.8871629 +lat_0=42.73884025 +ellps=WGS84"
proj4string(data3)
"[1]  +proj=moll +ellps=WGS84"
}

\author{Marco Smolla}