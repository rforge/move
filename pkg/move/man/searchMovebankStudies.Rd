\name{searchMovebankStudies}

\alias{searchMovebankStudies}
\alias{searchMovebankStudies,character,MovebankLogin-method}
\alias{searchMovebankStudies,character,missing-method}

\docType{methods}

\title{Search for a study}

\description{Searches for a study within movebank}

\usage{
  searchMovebankStudies(x, login, sensor = FALSE)
}

\arguments{
  \item{x}{a character string to search within the Movebank study names}
  \item{login}{an object of the \code{\link{MovebankLogin-class}}, if empty you'll be asked to enter your username or password}
  \item{sensor}{logical, if TRUE you can search for studies using a certain sensor type. Default is FALSE (not implemented yet)}
}

\details{
The search function searches explicitly for the entered phrase. If you for example type 'Goose' it will not show you studies including 'goose'. So rather search for 'oose' to find both. 
}

\note{
See the 'browesMovebank' vignette (\href{http://move.r-forge.r-project.org/moveWEB/Downloads.html}{move website download section}) for more information about security and how to use Movebank from within R. 
}


\author{Marco Smolla}

\examples{
\dontshow{
searchMovebankStudies("MPIO", login=login) #returns all studies that include this exact term
}
}