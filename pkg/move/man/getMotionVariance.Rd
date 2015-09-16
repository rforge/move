\name{getMotionVariance}
\alias{getMotionVariance}
\alias{getMotionVariance,DBBMM-method}
\alias{getMotionVariance,DBBMMBurstStack-method}
\alias{getMotionVariance,DBBMMStack-method}
\alias{getMotionVariance,dBGBvarianceTmp-method}
\alias{getMotionVariance,dBMvarianceStack-method}
\alias{getMotionVariance,dBMvarianceTmp-method}
\alias{getMotionVariance,dynBGB-method}


\docType{methods}

\title{Returns the estimated motion variance}

\description{This function returns from an object where it has been calculated before}

\usage{
  getMotionVariance(x,...)
}

\arguments{
  \item{x}{A variance object or an UD object calculated using the dynamic Bivariate Gaussian Bridges or dynamic Brownian Bridges}
  \item{...}{Currently un used}
}

\author{Bart Kranstauber}


\seealso{
	\code{\link{brownian.bridge.dyn}}, \code{\link{dynBGB}}
}

\examples{
data(leroydbbmm)
data(dbbmmstack)
getMotionVariance(leroydbbmm)
  getMotionVariance(dbbmmstack)
}
