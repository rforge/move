\name{split}

\alias{split}
\alias{split,MoveStack,missing-method}

\docType{methods}

\title{Splitting a MoveStack}

\description{Splitting MoveStack in a list of Move objects}

\usage{
  \S4method{split}{MoveStack,missing}(x, f, drop=FALSE, ...)
}

\arguments{
  \item{x}{MoveStack object}
  \item{f}{not needed}
  \item{drop}{not needed}
  \item{...}{not needed}
}

\details{
  A MoveStack is splitted into a list of \link[=Move-class]{Move} objects by the trackIds of the given MoveStack. 
  To stack this list again \link{moveStack} can be used. 
}

\author{Marco Smolla}

\examples{
##we would need a moveStack object right here
}