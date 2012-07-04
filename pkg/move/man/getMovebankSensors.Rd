\name{getMovebankSensors}

\alias{getMovebankSensors}
\alias{getMovebankSensors,ANY,missing-method}
\alias{getMovebankSensors,character,MovebankLogin-method}
\alias{getMovebankSensors,missing,MovebankLogin-method}
\alias{getMovebankSensors,missing,missing-method}
\alias{getMovebankSensors,numeric,MovebankLogin-method}

\docType{methods}

\title{Information about Movebank sensors}

\description{getMovebankSensors returns information about sensors in a study.}
  
\usage{
  getMovebankSensors(study, login)
}

\arguments{
  \item{study}{a character string (study name) or the numeric study ID as it is stored on Movebank, if missing the function returns a data.frame with information about all sensor types in Movebank}
  \item{login}{an object of the \code{\link{MovebankLogin-class}}, if empty you'll be asked to enter your username or password}
}

\details{
  The function returns either information about all sensor types that are available on Movebank or the sensor IDs corresponding to to the animal IDs in a specific study.
}

\note{
See the 'browesMovebank' vignette (\href{http://move.r-forge.r-project.org/moveWEB/Downloads.html}{move website download section}) for more information about security and how to use Movebank from within R. 
}


\author{Marco Smolla}