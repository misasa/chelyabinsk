% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cbk.read.ionml.R
\name{cbk.read.ionml}
\alias{cbk.read.ionml}
\title{Read IONML file and return an ionlame}
\usage{
cbk.read.ionml(ionml.xml, representative_time = FALSE, verbose = TRUE)
}
\arguments{
\item{ionml.xml}{An IONML file created in advance using appropriate
script dedicated for device.}

\item{representative_time}{Flag to compress timelame into single
column (default: FALSE).}

\item{verbose}{Output debug info (default: TRUE).}
}
\value{
A dataframe with mean and time columns.
}
\description{
Read IONML file and return an ionlame
}
\examples{
ionmlfile <- cbk.path("ref_cpx_klb1@1.xml")
message(sprintf("The ionmlfile is located at |\%s|.",ionmlfile))
ionlame   <- cbk.read.ionml(ionmlfile)
}
\seealso{
\code{\link{ionml.convert.qtegracsv2ioncsv}},
  \code{ionml.convert.ioncsv}, and
  \url{https://github.com/misasa/casteml}
}
