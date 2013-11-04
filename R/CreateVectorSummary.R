#' Compute summary statistics for a vector
#' 
#' A helper function to compute common summary statistics 
#' for a vector of values and return a vector of statistics.
#'
#' @param x The input vector \code{x}
#'
#' @return stats A vector of statistics
#'
#' @keywords keywords
#'
#' @export
#' 
#' @examples
#' library(rWipeTest)
#' stats <- CreateVectorSummary(rnorm(100,mean=1.0, sd=1.5))
CreateVectorSummary <- function(x) {
  # create a vector of statistics
  # mean, sd, se, lcl, ucl, count
  v.min  <- min(x)
  v.mean <- mean(x)
  v.med  <- median(x)
  v.max  <- max(x)
  n <- length(x)
  v.sd <- sd(x)
  v.se <- qnorm(0.975)*v.sd/sqrt(n)
  v.lcl <- v.mean-v.se
  v.ucl <- v.mean+v.se
  
  stats <- c(v.min, v.mean, v.med, v.max, v.sd, v.se, v.lcl, v.ucl, n)
  names(stats) <- c("min","mean","median","max","std dev","std err", "lcl", "ucl", "nObs")
  stats
}
