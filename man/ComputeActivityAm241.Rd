\name{ComputeActivityAm241}
\alias{ComputeActivityAm241}
\title{Compute the activity for our Am241 standard}
\usage{
  ComputeActivityAm241(strAnaDate)
}
\arguments{
  \item{strAnaDate}{The analysis date in Y-M-D format
  \code{strAnaDate}}
}
\value{
  curActivity The current activity
}
\description{
  Compute the current activity for the Am241 standard
  certified on 2000-02-01 on the analysis date
}
\examples{
library(rWipeTest)
act <- ComputeActivityAm241("2013-11-01")
}
\keyword{keywords}

