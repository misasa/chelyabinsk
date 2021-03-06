% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cbk.lame.acqify.R
\name{cbk.lame.acqify}
\alias{cbk.lame.acqify}
\title{Create pmlame with acqlist from one with stonelist}
\usage{
cbk.lame.acqify(reflame, acqlist, stonify_regexp = "@[[:digit:]]+$",
  verbose = FALSE)
}
\arguments{
\item{reflame}{A pmlame of reference with row of stone such as
`ref_cpx_klb1' and column of chem such as `Sr' [g/g].}

\item{acqlist}{List of analysis such as `ref_cpx_klb1@1'.}

\item{stonify_regexp}{A regexp string to stonefy analyses. Matched
string will be stripped off from acqlist to have stonelist.}

\item{verbose}{Output debug info (default: FALSE).}
}
\value{
A pmlame with row of analysis and columns of chem [g/g].
}
\description{
Create pmlame with acqlist from one with stonelist
}
\examples{
reflame <- structure(list(Li = c(1.37e-06, 3.91e-06, 3.052649172e-06, 5e-06, 8.68e-06, 2.08e-05, 7.6402e-06), Li_error = c(NA, NA, NA, NA, NA, NA, 5.883e-07), SiO2 = c(0.5195, 0.5359, 0.5341, 0.499, 0.5133, 0.5899, 0.5305), SiO2_error = c(0.0014, 0.0019, 0.0021, 0.006, 0.0086, 0.0091, 0.0072), Sr = c(3.606e-05, 8.8e-05, 7.3724079e-05, 0.000389, 0.000178, 0.000107, 0.0009696), Sr_error = c(NA, NA, NA, 2.3e-05, NA, NA, 2.685e-05)), .Names = c("Li", "Li_error", "SiO2", "SiO2_error", "Sr", "Sr_error"), class = "data.frame", row.names = c("ref-cpx-klb1", "ref-cpx-sax33", "ref-cpx-sax39c", "ref-gl-bhvo2", "ref-gl-dr1a1", "ref-gl-tahiti", "ref-pl-bytownite-c"))
acqlist <- c("ref_cpx_klb1@25", "ref_cpx_klb1@26", "ref_cpx_klb1@27", "ref_cpx_klb1@28", "ref_cpx_klb1@29", "ref_cpx_sax33@25", "ref_cpx_sax33@26", "ref_cpx_sax33@27", "ref_cpx_sax33@28", "ref_cpx_sax33@29", "ref_cpx_sax39c@25", "ref_cpx_sax39c@26", "ref_cpx_sax39c@27", "ref_cpx_sax39c@28", "ref_cpx_sax39c@29", "ref_gl_bhvo2@25", "ref_gl_bhvo2@26", "ref_gl_bhvo2@27", "ref_gl_bhvo2@28", "ref_gl_bhvo2@29", "ref_gl_dr1a1@25", "ref_gl_dr1a1@26", "ref_gl_dr1a1@27", "ref_gl_dr1a1@28", "ref_gl_dr1a1@29", "ref_gl_tahiti@25", "ref_gl_tahiti@26", "ref_gl_tahiti@27", "ref_gl_tahiti@28", "ref_gl_tahiti@29", "ref_pl_bytownite_c@25", "ref_pl_bytownite_c@26", "ref_pl_bytownite_c@27", "ref_pl_bytownite_c@28", "ref_pl_bytownite_c@29")
pmlame1 <- cbk.lame.acqify(reflame,acqlist)
}
