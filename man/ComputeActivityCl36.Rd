\name{ComputeActivityCl36}
\alias{ComputeActivityCl36}
\title{Compute the activity for our Cl36 standard}
\usage{
  ComputeActivityCl36(strAnaDate)
}
\arguments{
  \item{strAnaDate}{The analysis date in Y-M-D format
  \code{strAnaDate}}
}
\value{
  curActivity The current activity
}
\description{
  Compute the current activity for the Cl36 standard
  certified on 2000-02-01 on the analysis date
}
\examples{
library(rWipeTest)
act <- ComputeActivityCl36("2013-11-01")
}
\keyword{keywords}

