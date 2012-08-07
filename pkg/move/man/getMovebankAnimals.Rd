\name{getMovebankAnimals}

\alias{getMovebankAnimals}
\alias{getMovebankAnimals,ANY,MovebankLogin-method}
\alias{getMovebankAnimals,ANY,missing-method}

\docType{methods}

\title{Animals, tags and IDs in a Movebank study}

\description{Returns the animals, their tags and IDs from a Movebank study}

\usage{
  getMovebankAnimals(study, login)
}

\arguments{
  \item{study}{a character string (study name) or the numeric study ID as it is stored on Movebank}
  \item{login}{an object of the \code{\link{MovebankLogin-class}}, if empty you'll be asked to enter your username or password}
}

\details{
The function returns a \code{data.frame} that incldues the \code{animalID, animalName, id, sensor_type_id} and \code{tag_id} from the requested study. 
}

\note{
See the 'browesMovebank' vignette (\href{http://move.r-forge.r-project.org/moveWEB/Downloads.html}{move website download section}) for more information about security and how to use Movebank from within R. 
}


\author{Marco Smolla}