\name{getMovebankStudies}

\alias{getMovebankStudies}
\alias{getMovebankStudies,MovebankLogin-method}
\alias{getMovebankStudies,missing-method}

\docType{methods}

\title{All studies on Movebank}

\description{Returns all studies available on Movebank}

\usage{
  getMovebankStudies(login)
}

\arguments{
  \item{login}{an object of the \code{\link{MovebankLogin-class}}, if empty you'll be asked to enter your username or password}
}

\details{
   Returns all studies available on Movebank.
}

\note{
See the 'browesMovebank' vignette (\href{http://move.r-forge.r-project.org/moveWEB/Downloads.html}{move website download section}) for more information about security and how to use Movebank from within R. 
}


\author{Marco Smolla}

\examples{
\dontrun{
getMovebankStudies(login=login)
}
}