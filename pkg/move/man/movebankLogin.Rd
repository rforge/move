\name{movebankLogin}

\alias{movebankLogin}
\alias{movebankLogin,character,character-method}
\alias{movebankLogin,character,missing-method}
\alias{movebankLogin,missing,character-method}
\alias{movebankLogin,missing,missing-method}
 
\docType{methods}

\title{
Login into Movebank
}

\description{
Create a curl object to use the get and search functions for Movebank
}

\usage{
\S4method{movebankLogin}{character,character}(username,password)
}

\arguments{
\item{username}{Your Movebank username}
\item{password}{Your Movebank password}
}

\details{
Use this function to login to Movebank. After you logged in, you can use the Movebank browsing functions from the move package
}

\note{
You need to create an object called "curl" with this function. This curl object is than used by the other Movebank browsing functions. 
}


\author{Marco Smolla }

\examples{
\dontrun{
curl <- movebankLogin{username="xxx", password="zzz"}
}
}