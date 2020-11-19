#' @title Return normalized element abundances
#'
#' @description Return normalized element abundances.  Note that only
#'   elements that exist both in pmlame and reflame are processed.
#'   See also "Geochemical Modelling..." by Janousek et al. (2015)
#' @param pmlame A pmlame with row of stone and column of chem [g/g].
#' @param reflame A pmlame of a reference.  This can be a pmlame of
#'   multiple references.
#' @param suffix_after_chem String to recognize column of errors.
#'   Feed "_error" when necessary.  As of February 18, 2017, this
#'   exists only for compatibility.
#' @param verbose Output debug info (default: FALSE).
#' @return A ref-normalized daraframe with only elements defined in
#'   ref.
#' @seealso \link{cbk.ref} and \link{cbk.periodic}
#' @export
#' @examples
#' dflame.csv <- cbk.path("20081202172326.hkitagawa_trace.dflame")
#' message(sprintf("The dflame.csv is located at |%s|.",dflame.csv))
#' pmlame     <- cbk.read.dflame(dflame.csv,"ppm")
#' reflame    <- cbk.ref("Boynton.1989","ppm")
#' cbk.lame.normalize(pmlame,reflame)
cbk.lame.normalize <- function(pmlame,reflame,suffix_after_chem=NULL,verbose=FALSE){
  ## filter name when number of elements in reflame exceeds those in sample
  ## typically suffix_after_chem is "_error"

  stonelist0        <- rownames(pmlame)        # keep original name
  rownames(pmlame)  <- gsub("-","_",rownames(pmlame))
  rownames(reflame) <- gsub("-","_",rownames(reflame))

  meanlame0  <- cbk.lame.regulate(pmlame,mean=T,error=F,extra=F)
  errorlame0 <- cbk.lame.fetch.error(pmlame)
  reflame0   <- cbk.lame.regulate(reflame,mean=T,error=F,extra=F)

  chem       <- intersect(colnames(reflame0),colnames(meanlame0))

  if (verbose) {
    cat(file=stderr(),"cbk.lame.normalize:32: reflame0 <-",cbk.lame.dump(reflame0,show=F),"\n")
    cat(file=stderr(),"cbk.lame.normalize:33: meanlame0 <-",cbk.lame.dump(meanlame0,show=F),"\n")
    cat(file=stderr(),"cbk.lame.normalize:34: errorlame0 <-",cbk.lame.dump(errorlame0,show=F),"\n")
    cat(file=stderr(),"cbk.lame.normalize:35: chem <-",cbk.lame.dump(chem,show=F),"\n")
  }

  if (nrow(reflame) == 1){ # in case divide by CI chondrite
    reflame1   <- cbk.lame.rep(reflame0[,chem],nrow(pmlame),direction='v')
  } else if (nrow(pmlame) == nrow(reflame)) { # accept multi-row reflame
    reflame1   <- reflame0[,chem]
  } else {
    cat(file=stderr(),"cbk.lame.normalize:43: rownames(pmlame) <-",cbk.lame.dump(rownames(pmlame),show=F),"\n")
    cat(file=stderr(),"cbk.lame.normalize:44: rownames(reflame) <-",cbk.lame.dump(rownames(reflame),show=F),"\n")
    stop(cat(file=stderr(),"The rownames of pmlame and reflame are inconsistent.\n"))
  }

  if(is.null(suffix_after_chem)){
    meanlame1    <- meanlame0[,chem,drop=F]
    meanlame2    <- meanlame1 / reflame1

    ## when errorlame is significant
    if (nrow(cbk.lame.drop.dharma(errorlame0)) > 0) {
      errorlame1 <- errorlame0[,chem]
      errorlame2 <- errorlame1 / reflame1
      pmlame2    <- cbk.lame.merge.error(meanlame2,errorlame2)
    } else {
      pmlame2    <- meanlame2
    }
  } else { # This is for backward compatibility
    pmlame1      <- pmlame[,paste0(chem,suffix_after_chem)]
    pmlame2      <- pmlame1 / reflame1
  }

  rownames(pmlame2) <- stonelist0       # restore original name
  return(pmlame2) # data.frame to be consistent
}
