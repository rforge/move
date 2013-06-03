\name{getMovebankAnimals}

\alias{getMovebankAnimals}
\alias{getMovebankAnimals,ANY,MovebankLogin-method}
\alias{getMovebankAnimals,numeric,MovebankLogin-method}
\alias{getMovebankAnimals,character,MovebankLogin-method}
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
\code{getMovebankAnimals} belongs to the Movebank browsing functions and returns a \code{data.frame} that includes the \code{animalID, animalName, id, sensor_type_id} and \code{tag_id} from the requested study. 
}

\note{
See the 'browseMovebank' vignette (\href{http://computational-ecology.com/images/Move_Package/browseMovebank.pdf}{move website download section}) for more information about security and how to use Movebank from within R. 
}

\author{Marco Smolla}

\examples{
\dontrun{
getMovebankAnimals(study=82207, login=login)
}
}
