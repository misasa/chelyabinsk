% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cbk.lame.dump.R
\name{cbk.lame.dump}
\alias{cbk.lame.dump}
\title{Dump pmlame as string}
\usage{
cbk.lame.dump(pmlame, format = NULL, show = TRUE, verbose = FALSE,
  name = deparse(substitute(pmlame)))
}
\arguments{
\item{pmlame}{A pmlame with row of stone and column of chem [g/g].}

\item{format}{Format specifier that is passed to \code{casteml
convert}.}

\item{show}{Show string in console.}

\item{verbose}{Output debug info (default: FALSE).}

\item{name}{Name of the pmlame used with option `show'.}
}
\value{
A pmlame expressed in text
}
\description{
Dump pmlame as string
}
\examples{
pmlame0 <- structure(list(SiO2 = c(0.59, 0.52), Li = c(2.08e-05, 1.37e-06), Sr = c(0.000107, 3.61e-05)), row.names = c("ref-gl-tahiti", "ref-cpx-klb1"), .Names = c("SiO2", "Li", "Sr"), class = "data.frame")
cbk.lame.dump(pmlame0)
cbk.lame.dump(pmlame0,'isorg')
}
