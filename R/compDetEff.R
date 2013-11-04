#' Compute the detector efficiency
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


compDetEff <- function(str.ana, str.wd, dat.dir){
  setwd(str.wd)
  str.ana.file <- paste(dat.dir,'wipeTest.RData')
  load(str.ana.file)
  str.det.eff.file <- '../dat/LastDetEff.RData'

  pdf.dir  <- '../Sweave/pdf/'
  tab.dir  <- '../Sweave/tab/'
  # make sure these exist
  dir.create(pdf.dir, showWarnings = F, recursive = T)
  dir.create(tab.dir, showWarnings = F, recursive = T)

  n.alpha <- nrow(alpha.stds)

  if(n.alpha > 3){
    # compute and plot the detector efficiency, and
    # save the last values

    # compute the alpha detector efficiency
    a <- ComputeActivityAm241(alpha.stds$acq.date)

    alpha.e.mean <- alpha.stds[,4]/a
    alpha.e.se   <- alpha.stds[,5]/a
    alpha.stds[,8] <- alpha.e.mean
    alpha.stds[,9] <- alpha.e.se
    names(alpha.stds) = c("batch","id","acq.date","alpha.cpm","alpha.se",
                          "beta.cpm","beta.se","alpha.e.mean","alpha.e.se")

    print(alpha.stds)
    
    print(beta.stds)


    # compute the beta detector efficiency
    a <- ComputeActivityCl36(beta.stds$acq.date)

    beta.e.mean <- beta.stds[,6]/a
    beta.e.se   <- beta.stds[,7]/a
    beta.stds[,8] <- beta.e.mean
    beta.stds[,9] <- beta.e.se
    names(beta.stds) = c("batch","id","acq.date","alpha.cpm","alpha.se",
                          "beta.cpm","beta.se","beta.e.mean","beta.e.se")


    # plot once for graphics window
    df.det.eff <- eff.panel.plot(alpha.stds, beta.stds)

    # plot a second time for the pdf file
    str.det.eff.pdf <-paste(pdf.dir, str.ana,"-det-efficiency.pdf", sep="")
    pdf.options(useDingbats=TRUE)
    pdf(file="temp.pdf", width=9, height=6, pointsize=12)
    eff.panel.plot(alpha.stds, beta.stds)
    # Turn off device driver (to flush output to PDF)
    dev.off()
    # embed the fonts
    embedFonts("temp.pdf","pdfwrite", str.det.eff.pdf)
    unlink("temp.pdf")


    print(df.det.eff)
    str.tex   <- paste(tab.dir, 'det-eff.tex', sep='')
    str.label <- 'tab:DetEff'
    str.align <- '|l|r|r|'
    str.caption <- paste('Measured Tennelec Detector Efficiency')
    xt.dig <- c( 3, 3, 3)
    xt <- xtable(df.det.eff, digits=xt.dig, caption=str.caption,
               label=str.label, align=str.align)
  
    sink(str.tex)
    print(xt, include.rownames=T)
    sink()
  
    str.ei <- '\\endinput'
    cat(str.ei, file=str.tex, sep='\n', append=T)
  
    str.det.eff.name <- str.ana

   save(df.det.eff, str.det.eff.name, file=str.det.eff.file)

  } else{
    # use the last one
    load(str.det.eff.file)
    str.tex   <- paste(tab.dir, 'det-eff.tex', sep='')
    str.label <- 'tab:DetEff'
    str.align <- '|l|r|r|'
    str.caption <- paste('Previous Tennelec Detector Efficiency')
    xt.dig <- c( 3, 3, 3)
    xt <- xtable(df.det.eff, digits=xt.dig, caption=str.caption,
                 label=str.label, align=str.align)
  
    sink(str.tex)
    print(xt, include.rownames=T)
    sink()
  
    str.ei <- '\\endinput'
    cat(str.ei, file=str.tex, sep='\n', append=T)
  }
  str.tex   <- paste(tab.dir, 'det-eff.tex', sep='')
  str.label <- 'tab:DetEff'
  str.align <- '|l|r|r|'
  str.caption <- paste('Measured Tennelec Detector Efficiency from',
                       str.det.eff.name)
  xt.dig <- c( 3, 3, 3)
  xt <- xtable(df.det.eff, digits=xt.dig, caption=str.caption,
               label=str.label, align=str.align)

  sink(str.tex)
  print(xt, include.rownames=T)
  sink()

  str.ei <- '\\endinput'
  cat(str.ei, file=str.tex, sep='\n', append=T)

  # save what we need in the analysis file
  save(str.ana, alpha.stds, beta.stds,
       blanks, samples, df.det.eff,
       file=str.ana.file)
}

