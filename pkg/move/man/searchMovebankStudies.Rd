\name{searchMovebankStudies}

\alias{searchMovebankStudies}
\alias{searchMovebankStudies,character,MovebankLogin-method}
\alias{searchMovebankStudies,character,missing-method}

\docType{methods}

\title{Search within the Movebank database}

\description{The coordinates method extracts the coordinates of the track within a Move object.}

\usage{
  searchMovebankStudies(x, login, sensor = FALSE)
}

\arguments{
  \item{x}{a character string to search within the Movebank study names}
  \item{login}{...}
  \item{sensor}{logical, if TRUE you can search for studies using a certain sensor type. Default is FALSE}
}

\details{
The search function searches explicitly for the entered phrase. If you for example type 'Goose' it will not show you studies including 'goose'. So rather search for 'oose' to find both. 
}


\author{Marco Smolla}