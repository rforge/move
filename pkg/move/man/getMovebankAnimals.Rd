\name{getMovebankAnimals}

\alias{getMovebankAnimals}
\alias{getMovebankAnimals,character,CURLHandle-method}
\alias{getMovebankAnimals,numeric,CURLHandle-method}
\alias{getMovebankAnimals,ANY,missing-method}

\docType{methods}

\title{Which animals, tags and IDs are included in a specific study}

\description{Returns the animals, their tags and IDs from a study.}

\usage{
  getMovebankAnimals(study, login, ...)
}

\arguments{
  \item{study}{a character string (study name) or the numeric study ID as it is stored on Movebank}
  \item{login}{...}
  \item{...}{...}
}

\details{
The function returns a \code{data.frame} that incldues the \code{animalID, animalName, id, sensor_type_id} and \code{tag_id} from the requested study. 
}


\author{Marco Smolla}