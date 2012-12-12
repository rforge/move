\name{MovebankLogin-class}

\alias{MovebankLogin}
\alias{MovebankLogin-class}

\docType{class}

\title{
  The MovebankLogin class
}

\description{
  The MovebankLogin object is needed for every Movebank browsing function. Alternatively, one can also chose to enter the username and password every time one uses one of the browsing functions. 
}

\section{Slots}{
  \describe{
    \item{username}{username as character}
    \item{password}{password as character}
    \item{rcurl}{logical value, whether RCurl is installed or not}

  }
}

\section{Methods}{
  \describe{
    \item{\code{\link{movebankLogin}}}{\code{signature(object = "character")}: creates a MovebankLogin object}}
}

\author{Marco Smolla}

\keyword{classes}