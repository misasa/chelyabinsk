#' Return mean value of each element
#' @param pmlame A pmlame with rows of stone and columns of chem
#' @return A pmlame with mean value of each column
#' @export
#' @examples
#' pmlame  <- cbk.read.casteml(cbk.path("20081202172326.hkitagawa.pml"))
#' pmlame0 <- pmlame[,colnames(pmlame) != "sample_id"]
#' pmlame1 <- cbk.lame.colMeans(pmlame0)
cbk.lame.colMeans <- function(pmlame) {
  pmlame0 <- data.frame(t(colMeans(pmlame,na.rm=TRUE)))
  return(pmlame0)
}