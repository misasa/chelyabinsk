#' Remove rows where all values are NA
#' @param pmlame A pmlame with row of stone and column of chem [g/g].
#' @param column Flag to remove columns
#' @param verbose Output debug info (default: FALSE).
#' @return A pmlame without removed rows
#' @export
#' @examples
#' pmlfile <- cbk.path("20081202172326.hkitagawa.pml")
#' message(sprintf("The pmlfile is located at |%s|.",pmlfile))
#' pmlame  <- cbk.read.casteml(pmlfile,"ppm",category=NULL)
#' pmlame1 <- cbk.lame.drop.dharma(pmlame)
cbk.lame.drop.dharma <- function(pmlame,column=FALSE,verbose=FALSE) {
  ## repalce space by NA
  pmlame0   <- apply(pmlame, c(1,2), function(x) gsub("^$|^ $", NA, x))
  
  ## find lines only with NA
  blank_row <- apply(pmlame0, 1, function(x) all(is.na(x)))

  ## remove useless lines
  pmlame1   <- pmlame[!blank_row, ,drop=F]

  ## filter columns out
  if (column) {
    blank_col <- apply(pmlame0, 2, function(x) all(is.na(x)))
    pmlame1   <- pmlame1[ ,!blank_col,drop=F]
  }
  
  return(pmlame1)
}
