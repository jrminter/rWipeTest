#' Compute the standard error of a vector
#' 
#' A helper function to compute the standard error
#'
#' @param x The vector \code{x}
#'
#' @return stdError
#'
#' @keywords keywords
#'
#' 
#' @examples
#' library(rWipeTest)
stderr <- function(x) sqrt(var(x)/length(x))
