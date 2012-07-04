\name{getMovebankID}

\alias{getMovebankID}
\alias{getMovebankID,character,MovebankLogin-method}
\alias{getMovebankID,character,missing-method}

\docType{methods}

\title{Study ID}

\description{Returns the study ID of a certain study stored on Movebank}

\usage{
  getMovebankID(x,login)
}

\arguments{
  \item{x}{character, full name of the study, as stored on Movebank}
  \item{login}{an object of the \code{\link{MovebankLogin-class}}, if empty you'll be asked to enter your username or password}
}

\details{
  The getMovebankID function returns the ID of a study as it is stored on Movebank.org.
}

\note{
See the 'browesMovebank' vignette (\href{http://move.r-forge.r-project.org/moveWEB/Downloads.html}{move website download section}) for more information about security and how to use Movebank from within R. 
}


\author{Marco Smolla}