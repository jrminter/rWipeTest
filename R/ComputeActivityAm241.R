#' Compute the activity for our Am241 standard
#' 
#' Compute the current activity for the Am241 standard
#' certified on 2000-02-01 on the analysis date
#'
#' @param dt The analysis date in Y-M-D format \code{dt}
#'
#' @return nAct The current activity in dpm
#'
#' @keywords keywords
#'
#' @export
#' 
#' @examples
#' library(rWipeTest)
#' act <- ComputeActivityAm241("2013-11-01")
ComputeActivityAm241 <- function(dt) {
  # for Am-241 standard
  strOriDate <- c("9/11/1986  12:00:00 PM")   # For source no. 170-94
  nHalfLife <- 432.0                          # Am-241 half life
  nInitAct <- 217580.                         # dpm = 2.2 * 98900 pC
  theOriDate <- as.Date(strOriDate,format='%m/%d/%Y')
  theDiff <- as.Date(dt) - theOriDate
  nDecayDays <- as.numeric(theDiff)
  nDecayYrs <- nDecayDays / 365.25
  nFactor <- exp( -1.0 * nDecayYrs / nHalfLife)
  nAct <- nInitAct * nFactor
  nAct
}