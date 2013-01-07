\name{.unUsedRecords}

\alias{.unUsedRecords-class}
\alias{.unUsedRecordsStack-class}

\docType{class}

\title{.unUsedRecords and .unUsedRecordsStack class}

\description{The class .unUsedRecords and .unUsedRecordsStack is mostly an internal class that is made public to make inheritance easier. It is a basal class that stores unused records.}

\section{Slots}{
  \describe{
    \item{timestampsUnUsedRecords}{unused timestamps}
    \item{sensorUnUsedRecords}{unused sensor information}
    \item{dataUnUsedRecords}{further unused data}
  }
}

\author{Marco Smolla}
