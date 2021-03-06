#' @title Read CASTEML file and return a pmlame
#'
#' @description Read CASTEML file and return a pmlame.
#'
#' @details This function converts a CASTEML file to a csvfile by
#'   `cbk.convert.casteml()' and read it by `cbk.read.dflame()'.
#' @param pmlfile_or_stone A CASTEML file that exits locally or
#'   stone-ID.
#' @param tableunit Output unit that will be resolved by
#'   cbk.convector() (default: "none").
#' @param category Category specifier that is passed to \code{casteml
#'   convert}.
#' @param force Force read pmlfile with duplicated acquisitions
#'   (default: TRUE).
#' @param opts List of further options for plot.
#' @param verbose Output debug info (default: TRUE).
#' @return A dataframe with unit organized.
#' @seealso \code{\link{cbk.convert.casteml}},
#'   \code{\link{cbk.read.dflame}}, and
#'   \url{https://github.com/misasa/casteml}
#' @export
#' @examples
#' pmlfile <- cbk.path("20081202172326.hkitagawa.pml")
#' message(sprintf("The pmlfile is located at |%s|.",pmlfile))
#' pmlame  <- cbk.read.casteml(pmlfile,tableunit="ppm",category="trace")
#' stone   <- "20081202172326.hkitagawa"
#' pmlame  <- cbk.read.casteml(stone,tableunit="ppm",category="trace",force=TRUE)
cbk.read.casteml <- function(pmlfile_or_stone,opts=NULL,tableunit="none",category=NULL,force=TRUE,verbose=TRUE){
  opts_default <- list(Recursivep=FALSE)
  opts_default[intersect(names(opts_default),names(opts))] <- NULL  ## Reset shared options
  opts <- c(opts,opts_default)

  if (verbose) {
    cat(file=stderr(),
        "cbk.read.casteml:34: pmlfile_or_stone # =>",
        ifelse(is.data.frame(pmlfile_or_stone),"#<pmlame>",pmlfile_or_stone),"\n")
  }
  
  if (is.data.frame(pmlfile_or_stone)) { # pmlame fed
    pmlame     <- pmlfile_or_stone
  } else {
    if (file.exists(pmlfile_or_stone)) { # existing-pmlfile fed
      pmlfile  <- pmlfile_or_stone
    } else {                             # stone fed
      stone    <- pmlfile_or_stone
      if (opts$Recursivep) {
        # pmlfile  <- cbk.download.casteml(c("-R", stone))
        pmlfile  <- cbk.download.casteml(stone,Recursive=TRUE)
      } else {
        # pmlfile  <- cbk.download.casteml(c("-r", stone))
        pmlfile  <- cbk.download.casteml(stone,recursive=TRUE)
      }
    }
    dflame.csv <- cbk.convert.casteml(pmlfile,category=category)
    pmlame     <- cbk.read.dflame(dflame.csv,tableunit,force=force)
  }

  chemlist                    <- colnames(pmlame)
  property0                   <- cbk.periodic("atomicnumber")[chemlist] # atomicnumber, volatility, compatibility
  names(property0)            <- chemlist
  property0[is.na(property0)] <- 999
  property1                   <- sort(property0)
  pmlame                      <- pmlame[,names(property1),drop=FALSE]

  errorp                      <- grepl("_error",colnames(pmlame))
  if (any(errorp)) {
    chemlame  <- cbk.lame.regulate(pmlame,error=FALSE)
    extralame <- cbk.lame.regulate(pmlame,mean=FALSE,error=FALSE,extra=TRUE)
    errorlame <- cbk.lame.fetch.error(pmlame)
    pmlame    <- cbk.lame.merge.error(chemlame,errorlame)
    pmlame    <- cbind(pmlame, extralame)
  }
  return(pmlame)
}
