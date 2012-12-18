\name{getMovebankID}

\alias{getMovebankID}
\alias{getMovebankID,character,MovebankLogin-method}
\alias{getMovebankID,character,missing-method}

\docType{methods}

\title{Study ID}

\description{Returns the numeric study ID that corresponds to the character study NAME stored on Movebank}

\usage{
  getMovebankID(study,login)
}

\arguments{
  \item{study}{character, full name of the study, as stored on Movebank}
  \item{login}{an object of the \code{\link{MovebankLogin-class}}, if empty you'll be asked to enter your username or password}
}

\details{
  \code{getMovebankID} belongs to the Movebank browsing functions and returns the ID of a study as it is stored on Movebank.org.
}

\note{
See the 'browseMovebank' vignette (\href{http://computational-ecology.com/images/Move_Package/browseMovebank.pdf}{move website download section}) for more information about security and how to use Movebank from within R. 
}


\author{Marco Smolla}

\examples{
\dontrun{
getMovebankID(study="BCI Ocelot", login=login)
}
}