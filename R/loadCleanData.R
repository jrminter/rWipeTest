#' Load and clean the data and store the measurements in a data frame
#' 
#' A helper function to compute a panel plot of detector
#' efficiency for alpha and beta irradiation.
#'
#' @param batches A vector of batch numbers \code{batches}
#'
#' @param str.ana The name of the analysis \code{str.ana}
#'
#' @param str.wd Working directory for the analysis \code{str.wd}
#'
#' @param dat.dir Path to a data directory \code{dat.dir}
#'
#' @param odbcName Name for the access database \code{odbcName}
#'
#' @param bDebug A flag for verbose printing \code{bDebug}
#'
#' @return none
#'
#' @keywords keywords
#'
#' @export
#' 
#' @examples
#' library(rWipeTest)


loadCleanData <- function(batches, str.ana, str.wd, dat.dir, odbcName="Eclipse", bDebug=FALSE){
  setwd(str.wd)
  dir.create(dat.dir, showWarnings = F, recursive = T)
  str.ana.file <- paste(dat.dir,'wipeTest.RData')


  str.sql.one   <- "SELECT [Sample Aliquots].[Batch Key], [Sample Aliquots].[Aliquot Sample Key]," 
  str.sql.two   <- "[Sample Aliquots].[Creation Time], [Sample Aliquots].[Alias ID]"
  str.sql.three <- "FROM [Sample Aliquots] WHERE [Sample Aliquots].[Batch Key]="

  queryCounts <- function(df, j, chan=ch){
    the.id  <- as.character(df$alias.id[j])
    the.ask <- as.character(df$aliquot.sample.key[j])
    str.sql.acq.one <- " Select * FROM [LB Acquistion Data] where [Aliquot Sample Key] = "
    str.sql.acq  <- paste(str.sql.acq.one,the.ask,sep="")
    # print(str.sql.acq)
    dat <- sqlQuery(chan, str.sql.acq)
    dat
  }
  
  nBatch <- length(batches)
  ch <- odbcConnect(odbcName)
  raw.data <- NULL
  for ( i in  1:nBatch) {
    str.sql <- paste(str.sql.one, str.sql.two, str.sql.three,
                     batches[i], sep=" ")
    sql.dat <- sqlQuery(ch, str.sql)
    names(sql.dat) <- c("batch.key","aliquot.sample.key","creation.time",
                        "alias.id")
    dat.cts <- NULL
    nRows = nrow(sql.dat)
    for(j in 1:nRows){
      cts <- queryCounts(sql.dat, j)
      dat.cts <- rbind(dat.cts, cts)
    }
    sql.dat <- cbind(sql.dat, dat.cts)
    raw.data <- rbind(raw.data, sql.dat)
    if(!bDebug) rm(cts, dat.cts, sql.dat)  
  }
  close(ch)
  # to.rm <- c(2,3,5,6,7,9,14,15,16,17,18,20,21,22,23,24,25,26,27,28)
  to.rm <- c(2,3,5,6,7,9,14:28)

  if(!bDebug) print(names(raw.data))
  # print(head(raw.data))
  raw.data <- raw.data[,-to.rm]
  if(!bDebug)  print(names(raw.data))
  if(!bDebug)  print(head(raw.data))
  raw.data[,4] <- 60*raw.data[,4] # convert from cps to cpm
  raw.data[,5] <- 60*raw.data[,5]
  raw.data[,6] <- 60*raw.data[,6]
  raw.data[,7] <- 60*raw.data[,7]

  names(raw.data) <- c('batch', 'id', 'acq.date',
                       'alpha.cpm', 'alpha.se',
                       'beta.cpm', 'beta.se') 
  # ,
  # 'live.sec')
  print(head(raw.data))

  str.file <- paste(dat.dir, str.ana, '-raw.R', sep='')
  dput(raw.data, str.file)

  save( str.ana, file=str.ana.file)
}

