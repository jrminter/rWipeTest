#' Compute the activity for our Cl36 standard
#' 
#' Compute the current activity for the Cl36 standard
#' certified on 2000-02-01 on the analysis date
#'
#' @param strAnaDate The analysis date in Y-M-D format \code{strAnaDate}
#'
#' @return curActivity The current activity 
#'
#' @keywords keywords
#'
#' @export
#' 
#' @examples
#' library(rWipeTest)
#' act <- ComputeActivityCl36("2013-11-01")
ComputeActivityCl36 <- function( strAnaDate)
{
   # for Cl-36 standard
   strOriDate <- c("2/1/2000  12:00:00 PM")   # For source no. RR-659
   nHalfLife <- 3.01e+05                      # Cl-36 half life
   nInitAct <- 27310.                         # dpm
   theOriDate <- as.Date(strOriDate,format='%m/%d/%Y')
   theAnaDate <- as.Date(strAnaDate,format=  "%Y-%m-%d")
   theDiff <- theAnaDate - theOriDate
   nDecayDays <- as.numeric(theDiff)
   nDecayYrs <- nDecayDays / 365.25
   nFactor <- exp( -1.0 * nDecayYrs / nHalfLife)
   nAct <- nInitAct * nFactor
   nAct
}