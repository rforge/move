\name{getMovebankStudy}

\alias{getMovebankStudy}
\alias{getMovebankStudy,ANY,MovebankLogin-method}
\alias{getMovebankStudy,ANY,missing-method}

\docType{methods}

\title{Returns study information}

\description{This function returns information about the requested study like the authors of that study, licence type, citation and more.}

\usage{
  getMovebankStudy(study, login)
}

\arguments{
  \item{study}{a character string (study name) or the numeric study ID as it is stored on Movebank}
  \item{login}{an object of the \code{\link{MovebankLogin-class}}, if empty you'll be asked to enter your username or password}
}

\note{
See the 'browesMovebank' vignette (\href{http://move.r-forge.r-project.org/moveWEB/Downloads.html}{move website download section}) for more information about security and how to use Movebank from within R. 
}


\author{Marco Smolla}