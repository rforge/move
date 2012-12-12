\name{getMovebankData}

\alias{getMovebankData}
\alias{getMovebankData,ANY,ANY,MovebankLogin-method}

\docType{methods}

\title{Download data from Movebank}

\description{getMovebankData downloads the location and timestamp columns of a study stored in Movebank}

\usage{
  getMovebankData(study, animalName, login, ...)
}

\arguments{
  \item{study}{character, full name of the study, as stored on Movebank}
  \item{animalName}{character, single or a vector, with the name of the individuals as stored on Movebank}
  \item{login}{a \code{\link[=MovebankLogin-class]{MovebankLogin}}, if empty you'll be asked to enter your username or password}
  \item{...}{passing on additional arguments}
}

\details{
\code{getMovebankData} belongs to the Movebank browsing functions and returns a \link[=Move-class]{Move} object from studies with only on animal or \link[=MoveStack-class]{MoveStack} object for studies with multiple animals. If only a single or several particular animals of a study should be downloaded, a character vector can be provided for the \code{animalName} argument.\cr
  Remember that you need an account at Movebank.org, see \code{\link{movebankLogin}}.
}

\note{
See the 'browseMovebank' vignette (\href{http://computational-ecology.com/images/Move_Package/browseMovebank.pdf}{move website download section}) for more information about security and how to use Movebank from within R. 
}


\author{Marco Smolla}

\examples{
\dontrun{
getMovebankData(study="BCI Ocelot", login=login) #returns a MoveStack object from the specified study
getMovebankData(study="BCI Agouti", login=login) #returns a Move object (there is only one individual in this study)
getMovebankData(study=123413, animalName=c("Mancha","Yara"), login=login) #returns a MoveStack with two individuals
}
}