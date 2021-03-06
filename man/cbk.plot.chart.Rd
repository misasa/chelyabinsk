% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cbk.plot.chart.R
\name{cbk.plot.chart}
\alias{cbk.plot.chart}
\title{Visualze IONML as intensity chart}
\usage{
cbk.plot.chart(IONML_or_pmlame, opts = NULL, t2 = 25, t3 = 60,
  ref = "int_29Si", acq = "", w_size = 10, verbose = FALSE, ...)
}
\arguments{
\item{IONML_or_pmlame}{IONML (xml file) or ion-type pmlame (R object).}

\item{opts}{List of further options for plot.  See \link{cbk.plot}.}

\item{t2}{When ion starts (default: 25 s).}

\item{t3}{When ion ends (default: 60 s).}

\item{ref}{Reference ion species (default: "int_29Si").}

\item{acq}{Name of analysis such as `ref_cpx_klb1@1' (default: "").}

\item{w_size}{Window size for moving average (default: 10).}

\item{verbose}{Output debug info (default: FALSE).}

\item{...}{Graphical options that are passed to matplot and legend,
such as `col' and `pch'.}
}
\value{
@return A pmlame used to plot the diagram.
}
\description{
Visualze IONML as intensity chart.  This reads IONML
  or ion-type pmlame (R object) and plots intensity chart of
  elements.  Two intensity charts will be droawn with and without
  normalization by reference ion intensity.
}
\examples{
ionml.xml <- cbk.path("ref_cpx_klb1@1.xml")
message(sprintf("The ionml.xml is located at |\%s|.",ionml.xml))
cbk.plot.chart(ionml.xml,acq="ref_cpx_klb1@1")

pmlame <- cbk.read.ionml(ionml.xml,representative_time=TRUE)
cbk.plot.chart(pmlame)
}
