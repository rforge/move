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
Creates an object that can be used with all Movebank browsing functions. 
}

\usage{
\S4method{movebankLogin}{character,character}(username,password)
}

\arguments{
\item{username}{Your Movebank username}
\item{password}{Your Movebank password}
}

\details{
Use this function to login to Movebank. After you logged in, you can use the Movebank browsing functions from the move package. 
}

\note{
If you do not have the RCurl package installed \code{movebankLogin} will store your username and password in the object you assign it to. Furthermore, if you have no RCurl a http connection is used to retrieve data from Movebank instead of the more secure https protocol.  

}


\author{Marco Smolla }

\examples{
\dontrun{
##first create the login object 
login <- movebankLogin{username="xxx", password="zzz"}

##and than use it with Movebank browsing functions
getMovebankStudies(login)
}
}