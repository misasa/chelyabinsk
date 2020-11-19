#' Suggest categories to plot
#' @param pmlfile_or_stone A CASTEML file that exits locally or
#'   stone-ID (or pmlame) to survey categories.  Default is NULL and
#'   return all categories regardless their plotability.
#' @return Categories in preferred order
#' @export
#' @examples
#' pmlfile  <- cbk.path("20081202172326.hkitagawa.pml")
#' message(sprintf("The pmlfile is located at |%s|.",pmlfile))
#' category <- cbk.category(pmlfile)
#' category <- cbk.category()
cbk.category <- function(pmlfile_or_stone=NULL) {
  if(is.null(pmlfile_or_stone)){
    category <- c(
      "default",
      "trace",
      "spider",
      "REE",
      "lithium",
      "oxygen",
      "lead",
      "spots")
  } else {
    pmlame0   <- cbk.read.casteml(pmlfile_or_stone)
    pmlame    <- cbk.lame.drop.dharma(pmlame0,column=TRUE)
    ChemList  <- colnames(pmlame)
    REEList   <- c("La","Ce","Pr","Nd","Sm","Eu","Gd","Tb","Dy","Ho","Er","Tm","Yb","Lu")
    MajorList <- c("Na2O","MgO","Al2O3","SiO2","P2O5","K2O","CaO","TiO2","Cr2O3","MnO","FeO","Fe2O3","NiO","Na","Mg","Al","Si","P","K","Ca","Ti","Cr","Mn","Fe","Ni","H")
    OxyList   <- c("d18O","d17O")
    LiList    <- "d7Li"
    PbList    <- c("Pb206zPb204","Pb207zPb204","Pb208zPb204")
    SpotsList <- c("x_image","y_image")

    ### Suggest single category
    ## if (any(REEList %in% ChemList)) {
    ##   category <- "trace"
    ## } else if (any(OxyList %in% ChemList)) {
    ##   category <- "oxygen"
    ## } else if (any(LiList %in% ChemList)) {
    ##   category <- "lithium"
    ## } else if (any(PbList %in% ChemList)) {
    ##   category <- "lead"
    ## } else {
    ##   category <- "trace"
    ## }

    ### Suggest categories in preferred order
    category <- c()
    if (any(REEList %in% ChemList)) {
      category <- append(category, c("trace", "REE"))
    }
    if (any(MajorList %in% ChemList)) {
      category <- append(category, c("spider"))
    }
    if (all(OxyList %in% ChemList)) {
      category <- append(category, "oxygen")
    }
    if (all(LiList %in% ChemList)) {
      category <- append(category, "lithium")
    }
    if (all(PbList %in% ChemList)) {
      category <- append(category, "lead")
    }
    if (all(SpotsList %in% ChemList)) {
      category <- append(category, "spots")
    }
  }
  return(category)
}
