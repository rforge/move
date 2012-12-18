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
   \code{getMovebankStudies} belongs to the Movebank browsing functions and returns a data.frame of all studies available on Movebank.
}

\note{
See the 'browseMovebank' vignette (\href{http://computational-ecology.com/images/Move_Package/browseMovebank.pdf}{move website download section}) for more information about security and how to use Movebank from within R. 
}


\author{Marco Smolla}

\examples{
\dontrun{
getMovebankStudies(login=login)
}
}