\name{getMovebankSensors}

\alias{getMovebankSensors}
\alias{getMovebankSensors,ANY,missing-method}
\alias{getMovebankSensors,ANY,MovebankLogin-method}
\alias{getMovebankSensors,missing,MovebankLogin-method}
\alias{getMovebankSensors,numeric,MovebankLogin-method}
\alias{getMovebankSensors,character,MovebankLogin-method}
\alias{getMovebankSensors,missing,missing-method}

\docType{methods}

\title{Information about Movebank sensors}

\description{getMovebankSensors returns information about sensors in a study.}
  
\usage{
  getMovebankSensors(study, login)
}

\arguments{
  \item{study}{a character string (study name) or the numeric study ID as it is stored on Movebank}
  \item{login}{an object of the \code{\link{MovebankLogin-class}}, if empty you'll be asked to enter your username or password}
}

\details{
  \code{getMovebankSensors} belongs to the Movebank browsing functions and returns either information about all sensor types that are available on Movebank (if the study argument is missing) or the sensor IDs corresponding to to the animal IDs in a specific study.
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
getMovebankSensors(study=123413, login=login)
}
}
