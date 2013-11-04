#' Create a panel box plot of activity 
#' 
#' A helper function to compute a panel box plot of activity
#' and return a list of outliers.
#'
#' @param a The input vector of alpha counts (pCi) data  \code{a}
#'
#' @param b The input vector of beta counts (pCi) \code{b}
#'
#' @return outliers A list with both alpha and beta outliers
#'
#' @keywords keywords
#'
#' 
#' @examples
#' library(rWipeTest)
#' 
activity.panel.boxplot<- function(a,b){
  # make a panel plot of boxplots of alpha and
  # beta counts and return lists of outliers
  par(mfrow = c(1,2))
  a.bp  <- boxplot(a, ylab=expression(alpha * " Activity (pCi)"))
  a.out <- a.bp$out
  
  b.bp <- boxplot(b, ylab=expression(beta * " Activity (pCi)"))
  b.out <- b.bp$out
  
  outliers <- list(alpha=a.out, beta=b.out)
  par(mfrow = c(1,1))
  
  outliers
}