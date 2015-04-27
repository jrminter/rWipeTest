#' Sort the data and store the measurements in a data frame
#' 
#' A helper function to compute a panel plot of detector
#' efficiency for alpha and beta irradiation.
#'
#' @param str.ana The name of the analysis \code{str.ana}
#'
#' @param str.wd Working directory for the analysis \code{str.wd}
#'
#' @param dat.dir Path to a data directory \code{dat.dir}
#'
#'
#' @return none
#'
#' @keywords keywords
#'
#' @export
#' 
#' @examples
#' library(rWipeTest)


sortAna <- function(str.ana, str.wd, dat.dir){
  setwd(str.wd)
  str.ana.file <- paste(dat.dir,'wipeTest.RData')
  load(str.ana.file)

  str.file <- paste(dat.dir, str.ana, '-raw.R', sep='')
  data <- dget(str.file)
  

  alpha.stds <- subset(data, data$id=="Am-241")
  beta.stds  <- subset(data, data$id=="Cl-36")
  blanks     <- subset(data, data$id=="blank")
  samp       <- subset(data, data$id!="Am-241")
  samp       <- subset(samp, samp$id!="Cl-36")
  samples    <- subset(samp, samp$id!="blank" )

  rm(samp, data)

  # save what we need in the analysis file
  save(str.ana, alpha.stds, beta.stds,
       blanks, samples,
       file=str.ana.file)
}

