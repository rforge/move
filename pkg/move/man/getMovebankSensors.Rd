\name{getMovebankSensors}

\alias{getMovebankSensors}
\alias{getMovebankSensors,ANY,missing-method}
\alias{getMovebankSensors,character,MovebankLogin-method}
\alias{getMovebankSensors,missing,MovebankLogin-method}
\alias{getMovebankSensors,missing,missing-method}
\alias{getMovebankSensors,numeric,MovebankLogin-method}

\docType{methods}

\title{Which sensors, tags and IDs are included in a specific study}

\description{This function returns all available sensor types in Movebank. If a study is specified it returns the animal ID correlated with the sensor type and tag ID.}

\usage{
  getMovebankSensors(study, login)
}

\arguments{
  \item{study}{a character string (study name) or the numeric study ID as it is stored on Movebank}
  \item{login}{...}
}

\details{
  The function returns a \code{data.frame} that incldues the \code{animalID, animalName, id, sensor_type_id} and \code{tag_id} from the requested study. 
}


\author{Marco Smolla}