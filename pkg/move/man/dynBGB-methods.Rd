\name{dynBGB}
\alias{dynBGB}
\alias{dynBGB,.MoveTrackSingle,RasterLayer,numeric,numeric-method}
\alias{dynBGB,.MoveTrackSingle,numeric,ANY,numeric-method}
\alias{dynBGB,dBGBvariance,RasterLayer,numeric,numeric-method}
\docType{methods}

\title{Calculation of the dynamic Bivariate Gausian Bridge}
\arguments{
  \item{move}{the move object or variance object used for
  calculating the ud if a \code{.MoveTrackSingle} object is
  supplied this is converted into a \code{dBGBvariance}
  object using the \code{dynBGBvariance} function}

  \item{raster}{either the raster used for UD
  calculation the resolution of the raster used for UD calculation}

  \item{locErr}{the location errors used for the
  calculation}

  \item{maxInt}{}
}
\description{
}

