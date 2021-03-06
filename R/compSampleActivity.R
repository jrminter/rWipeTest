#' Compute the sample activity
#' 
#' A helper function to compute the sample activity
#' prepare pdfs and tex files for the report
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
#' @import xtable
#'
#' @export
#' 
#' @examples
#' library(rWipeTest)


compSampleActivity <- function(str.ana, str.wd, dat.dir){
  setwd(str.wd)
  str.ana.file <- paste(dat.dir,'wipeTest.RData')
  
  # avoid R CMD CHECK issue
  alpha.stds <- NULL
  beta.stds <- NULL
  blanks <- NULL
  samples <- NULL
  df.det.eff <- NULL
  
  load(str.ana.file)

  pdf.dir       <- '../knitr/inc/pdf/'
  tab.dir       <- '../knitr/inc/tex/'
  dir.create(pdf.dir, showWarnings = F, recursive = T)
  dir.create(tab.dir, showWarnings = F, recursive = T)


  # calculate the blanks activity
  v.blanks.alpha.pCi <- round(blanks$alpha.cpm/(2.2*df.det.eff[1,1]), 3)
  v.blanks.alpha.se  <- round(blanks$alpha.se/(2.2*df.det.eff[1,1]), 3)
  v.blanks.beta.pCi  <- round(blanks$beta.cpm/(2.2*df.det.eff[1,2]), 3)
  v.blanks.beta.se   <- round(blanks$beta.se/(2.2*df.det.eff[1,2]), 3)

  blank.activity <- data.frame(batch=blanks$batch,
                               alpha.pCi=v.blanks.alpha.pCi,
                               alpha.se=v.blanks.alpha.se,
                               beta.pCi=v.blanks.beta.pCi,
                               beta.se=v.blanks.beta.se)
  rm(v.blanks.alpha.pCi, v.blanks.alpha.se,
     v.blanks.beta.pCi, v.blanks.beta.se)
  # print(blank.activity)

  # calculate the samples activity
  v.samples.alpha.pCi <- round(samples$alpha.cpm/(2.2*df.det.eff[1,1]), 3)
  v.samples.alpha.se  <- round(samples$alpha.se/(2.2*df.det.eff[1,1]), 3)
  v.samples.beta.pCi  <- round(samples$beta.cpm/(2.2*df.det.eff[1,2]), 3)
  v.samples.beta.se   <- round(samples$beta.se/(2.2*df.det.eff[1,2]), 3)

  sample.activity <- data.frame(batch=samples$batch,
                                id=samples$id,
                                alpha.pCi=v.samples.alpha.pCi,
                                alpha.se=v.samples.alpha.se,
                                beta.pCi=v.samples.beta.pCi,
                             beta.se=v.samples.beta.se)
  rm(v.samples.alpha.pCi, v.samples.alpha.se,
     v.samples.beta.pCi, v.samples.beta.se)

  # create summary

  v.blanks.alpha.pci  <- CreateVectorSummary(blank.activity$alpha.pCi)
  v.blanks.beta.pci   <- CreateVectorSummary(blank.activity$beta.pCi)
  v.samples.alpha.pci <- CreateVectorSummary(sample.activity$alpha.pCi)
  v.samples.beta.pci  <- CreateVectorSummary(sample.activity$beta.pCi)
  df.res <- data.frame(blanks.alpha=v.blanks.alpha.pci,
                       blanks.beta=v.blanks.beta.pci,
                       samples.alpha=v.samples.alpha.pci,
                       samples.beta=v.samples.beta.pci)

  rownames(df.res) <- c("min","mean", "median", "max",
                        "sd", "se", "lcl", "ucl", "n")
  rm(v.blanks.alpha.pci,
     v.blanks.beta.pci,
     v.samples.alpha.pci,
     v.samples.beta.pci)

  # write the summary output
  str.tex   <- paste(tab.dir, 'summary.tex', sep='')
  str.label <- 'tab:Summary'
  str.align <- '|l|r|r|r|r|'
  str.caption <- paste('Activity Summary (pCi)')
  xt.dig <- c( 3, 3, 3, 3, 3)
  xt <- xtable(df.res, digits=xt.dig, caption=str.caption,
               label=str.label, align=str.align)

  sink(str.tex)
  print(xt, include.rownames=T)
  sink()

  str.ei <- '\\endinput'
  cat(str.ei, file=str.tex, sep='\n', append=T)

  outliers <- activity.panel.boxplot(sample.activity$alpha.pCi,
                                     sample.activity$beta.pCi )

  alpha.outliers <- which(sample.activity[,3]%in% outliers$alpha)
  alpha.out <- sample.activity[alpha.outliers,]
  alpha.out <- alpha.out[order(-alpha.out[,3]), ]
  # print(alpha.out)

  str.boxplot.pdf <-paste(pdf.dir, str.ana,"-boxplot.pdf", sep="")
  # plot a second time for the pdf file
  pdf.options(useDingbats=TRUE)
  pdf(file="temp.pdf", width=9, height=6, pointsize=12)
  activity.panel.boxplot(sample.activity$alpha.pCi,
                         sample.activity$beta.pCi )
  # Turn off device driver (to flush output to PDF)
  dev.off()
  # embed the fonts
  embedFonts("temp.pdf","pdfwrite", str.boxplot.pdf)
  unlink("temp.pdf")

  # write the output
  str.tex   <- paste(tab.dir, 'alpha-outliers.tex', sep='')
  str.label <- 'tab:AlphaOut'
  str.align <- '|l|r|r|r|r|r|r|'
  str.caption <- paste('Alpha Outliers')
  xt.dig <- c( 1, 3, 3, 3, 3, 3, 3)
  xt <- xtable(alpha.out, digits=xt.dig, caption=str.caption,
               label=str.label, align=str.align)

  sink(str.tex)
  print(xt, include.rownames=F)
  sink()

  str.ei <- '\\endinput'
  cat(str.ei, file=str.tex, sep='\n', append=T)



  beta.outliers <- which(sample.activity[,5]%in% outliers$beta)
  beta.out <- sample.activity[beta.outliers,]
  beta.out <- beta.out[order(-beta.out[,5]), ]
  # print(beta.out)

  # write the output
  str.tex   <- paste(tab.dir, 'beta-outliers.tex', sep='')
  str.label <- 'tab:BetaOut'
  str.align <- '|l|r|r|r|r|r|r|'
  str.caption <- paste('Beta Outliers')
  xt.dig <- c( 1, 3, 3, 3, 3, 3, 3)
  xt <- xtable(beta.out, digits=xt.dig, caption=str.caption,
               label=str.label, align=str.align)

  sink(str.tex)
  print(xt, include.rownames=F)
  sink()

  str.ei <- '\\endinput'
  cat(str.ei, file=str.tex, sep='\n', append=T)


  # sort by ID
  sample.activity$id <- as.numeric(as.character(sample.activity$id))
  sample.activity <- sample.activity[order(sample.activity[,2]), ]
  # print(head(sample.activity))

  str.samples.csv <-paste(dat.dir,str.ana,
                          "-wipe-test-samples.csv", sep="")
  # write the samples to a csv file
  write.csv(sample.activity, file = str.samples.csv, row.names = FALSE )
  
  # save what we need in the analysis file
  save(str.ana, alpha.stds, beta.stds,
     blanks, samples, df.det.eff, outliers,
     file=str.ana.file)

}

