#' Create a panel plot of detector efficiency 
#' 
#' A helper function to compute a panel plot of detector
#' efficiency for alpha and beta irradiation.
#'
#' @param alpha The input data frame for alpha standards \code{alpha}
#'
#' @param beta The input data frame for alpha standards \code{beta}
#'
#' @return df.det.eff A data frame of detector efficiency
#'
#' @keywords keywords
#'
#' 
#' @examples
#' library(rWipeTest)
#' 
eff.panel.plot <- function(alpha, beta){
  # make a panel plot of detector efficiency
  # and return a data frame with the mean and se
  # given the data frames for the standards
  par(mfrow = c(1,2))
  a.mean <- mean(alpha[,8])
  a.se   <- stderr(alpha[,8])
  a.sd   <- sd(alpha[,8])
  x.t <- c(min(alpha[,1]), max(alpha[,1]))
  y.t <- c(a.mean-3*a.sd, a.mean+3*a.sd )
  plot(x.t, y.t, type='n', xlab='batch',
       ylab=expression(alpha * ' detector efficiency'))
  
  points(alpha[,1], alpha[,8], pch=19)

  abline(h=a.mean, col='red', lwd=2)
  abline(h=a.mean+a.se, col='red', lty=2, lwd=2)
  abline(h=a.mean-a.se, col='red', lty=2, lwd=2)
  abline(h=a.mean+1.96*a.sd, col='blue', lty=3, lwd=2)
  abline(h=a.mean-1.96*a.sd, col='blue', lty=3, lwd=2)
  
 
  b.mean <- mean(beta$beta.e.mean)
  b.se   <- stderr(beta$beta.e.mean)
  b.sd   <- sd(beta$beta.e.mean)
  x.t <- c(min(beta$batch), max(beta$batch))
  y.t <- c(b.mean-3*b.sd, b.mean+3*b.sd )
  plot(x.t, y.t, type='n', xlab='batch',
       ylab=expression(beta * ' detector efficiency'))
  points(beta$batch, beta$beta.e.mean, pch=19)
  abline(h=b.mean, col='red', lwd=2)
  abline(h=b.mean+b.se, col='red', lty=2, lwd=2)
  abline(h=b.mean-b.se, col='red', lty=2, lwd=2)
  abline(h=b.mean+1.96*b.sd, col='blue', lty=3, lwd=2)
  abline(h=b.mean-1.96*b.sd, col='blue', lty=3, lwd=2)
  
  par(mfrow = c(1,1))
  
  # make a data frame with mean and se
  df.det.eff <- data.frame(alpha=c(a.mean, a.se),
                           beta=c(b.mean, b.se))
  rownames(df.det.eff) <- c('mean', 'se')
  df.det.eff <- round(df.det.eff, 4)
  # return the data frame
  df.det.eff
}

