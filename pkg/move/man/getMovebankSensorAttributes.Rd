\name{getMovebankSensorsAttributes}

\alias{getMovebankSensorsAttributes}
\alias{getMovebankSensorsAttributes,ANY,MovebankLogin-method}
\alias{getMovebankSensorsAttributes,numeric,MovebankLogin-method}
\alias{getMovebankSensorsAttributes,character,MovebankLogin-method}

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
  \code{getMovebankSensorAttributes} belongs to the Movebank browsing functions and returns the attributes of the sensors of a study, i.e. what is the sensor id and which data types are stored for this sensor (e.g. GPS sensors store longitude and latitude locations, and timestamps and have 673 as their ID on Movebank).
}

\note{
See the 'browseMovebank' vignette (\href{http://computational-ecology.com/images/Move_Package/browseMovebank.pdf}{move website download section}) for more information about security and how to use Movebank from within R. 
}


\author{Marco Smolla}

\seealso{\code{\link{movebankLogin}}}
\examples{
\dontrun{
#obtain a login
login<-movebankLogin()
getMovebankSensorsAttributes(study=123413, login=login)
}
}
