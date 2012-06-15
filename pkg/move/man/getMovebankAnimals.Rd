\name{getMovebanAnimals}

\alias{getMovebanAnimals}
\alias{getMovebanAnimals,character,CURLHandle-method}
\alias{getMovebanAnimals,numeric,CURLHandle-method}
\alias{getMovebanAnimals,ANY,missing-method}

\docType{methods}

\title{Which animals, tags and IDs are included in a specific study}

\description{The coordinates method extracts the coordinates of the track within a Move object.}

\usage{
  getMovebanAnimals(x, login)
}

\arguments{
  \item{x}{a character string (study name) or the numeric study ID as it is stored on Movebank}
  \item{login}{...}
}

\details{
The function returns a \code{data.frame} that incldues the \code{animalID, animalName, id, sensor_type_id} and \code{tag_id} from the requested study. 
}


\author{Marco Smolla}