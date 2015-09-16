\name{n.indiv}

\alias{n.indiv}
\alias{n.indiv,Move-method}
\alias{n.indiv,.MoveTrackStack-method}

\docType{methods}

\title{Extract the number of individuals of a Move or MoveStack object}

\description{The n.indiv function returns the number of individuals from a Move or MovesStack object.}

\usage{
  \S4method{n.indiv}{Move}(obj)
  \S4method{n.indiv}{.MoveTrackStack}(obj)
}

\arguments{
  \item{obj}{Move or MoveStack object}
}
\details{The function returns 1 for objects of the class Move, and number of individuals for a moveStack}
\author{Bart Kranstauber}

\examples{
data(leroy)
n.indiv(leroy)
data(fishers)
n.indiv(fishers)
}
