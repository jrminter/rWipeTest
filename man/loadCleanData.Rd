\name{loadCleanData}
\alias{loadCleanData}
\title{Load and clean the data and store the measurements in a data frame}
\usage{
  loadCleanData(batches, str.ana, str.wd, dat.dir,
    odbcName = "Eclipse", bDebug = FALSE)
}
\arguments{
  \item{batches}{A vector of batch numbers \code{batches}}

  \item{str.ana}{The name of the analysis \code{str.ana}}

  \item{str.wd}{Working directory for the analysis
  \code{str.wd}}

  \item{dat.dir}{Path to a data directory \code{dat.dir}}

  \item{odbcName}{Name for the access database
  \code{odbcName}}

  \item{bDebug}{A flag for verbose printing \code{bDebug}}
}
\value{
  none
}
\description{
  A helper function to compute a panel plot of detector
  efficiency for alpha and beta irradiation.
}
\examples{
library(rWipeTest)
}
\keyword{keywords}

