\name{as.data.frame}

\alias{as.data.frame,Move-method}

\docType{methods}

\title{Return a Data Frame}

\description{Function to create a data.frame with the spatial data frame information from a Move object.}

\usage{
  \S4method{as.data.frame}{Move}(x,...)
}

\arguments{
  \item{x}{a Move object}
  \item{...}{additional arguments to be passed to or from methods}
}

\details{
  as.data.frame extracts the sdf argument from a Move object.
}

\examples{

## create a move object
data <- move(system.file("data/leroy_fisher_LaPoint.csv",package="move"),proj=CRS("+proj=longlat"))

## returns a data.frame with all information stored in the spatial data frame of the move object
df <- as.data.frame(data)
}

\author{Marco Smolla}
