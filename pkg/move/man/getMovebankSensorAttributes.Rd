\name{getMovebankSensorsAttributes}

\alias{getMovebankSensorsAttributes}
\alias{getMovebankSensorsAttributes,character,MovebankLogin-method}
\alias{getMovebankSensorsAttributes,numeric,MovebankLogin-method}

\docType{methods}

\title{Available sensor attributes}

\description{This function returns all attributes of the sensors of the requested study.}

\usage{
  getMovebankSensorsAttributes(study, login)
}

\arguments{
  \item{study}{a character string (study name) or the numeric study ID as it is stored on Movebank}
  \item{login}{an object of the \code{\link{MovebankLogin-class}}, if empty you'll be asked to enter your username or password}
}

\details{
  Returns the attributes of the sensors of a study, i.e. which sensor has which id and which data stored (e.g. GPS sensors store longitute and latitude locations, and timestamps and have 673 as their ID on Movebank).
}

\note{
See the 'browesMovebank' vignette (\href{http://move.r-forge.r-project.org/moveWEB/Downloads.html}{move website download section}) for more information about security and how to use Movebank from within R. 
}


\author{Marco Smolla}