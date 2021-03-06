% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cbk.download.image.R
\name{cbk.download.image}
\alias{cbk.download.image}
\title{Download imagefile from a surface}
\usage{
cbk.download.image(pmlfile_or_surface, outfile = NULL, force = FALSE,
  directAuth = NULL, recursive = FALSE)
}
\arguments{
\item{pmlfile_or_surface}{Unique ID of surface. Really, surface can
be pmlfile, stone, and bib although only the first image will be
downloaded.}

\item{outfile}{Path to save a imagefile. Unless specified,
imagename of Medusa will be located on current directory.}

\item{force}{Download again and overwrite preexisting local
imagefile.}
}
\value{
Path to the imagefile downloaded
}
\description{
Download imagefile from a surface. This function
  download CASTEML of a surface, parse them and extract attachment
  tag in the first spot, and download to local directory. When
  outfile exists, this function does not download a new imagefile.
}
\examples{
directAuth <- list(uri="https://dream.misasa.okayama-u.ac.jp/pub/")
pmlfile <- cbk.download.casteml("20160819165624-372633",directAuth=directAuth,force=TRUE)
imagefile <- cbk.download.image(pmlfile,directAuth=directAuth)

imagefile <- cbk.download.image("20160819165624-372633",directAuth=directAuth,force=TRUE)
}
\seealso{
\code{casteml download},
  \url{https://github.com/misasa/casteml}, and
  \code{\link{cbk.convert.casteml}}
}
