\name{as.data.frame}

\alias{as.data.frame}
\alias{as.data.frame,Move-method}

\docType{methods}

\title{Return a Data Frame}

\description{Function to create a data.frame with the information of a spatial data frame contained in the Move object.}

\usage{
  \S4method{as.data.frame}{Move}(x,...)
}

\arguments{
  \item{x}{an object of the \link{Move-class}}
  \item{...}{additional arguments to be passed to or from methods}
}

\details{
  as.data.frame extracts the sdf argument from a Move object (see \code{\link{Move-class}})
}

\examples{
## create a move object
data(leroy)
data <- leroy[99:150,]

## returns a data.frame with all information stored in the spatial data frame of the move object
head(df <- as.data.frame(data))
\dontshow{
data(fishers)
head(data.frame(fishers))
}
}

\author{Marco Smolla}
