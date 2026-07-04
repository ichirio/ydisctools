% Generated manually for ydisctools (hand-maintained; keep in sync with the
% roxygen block in R/ars_toc.R).
\name{ars_toc_template}
\alias{ars_toc_template}
\title{Write a TFL TOC workbook template}
\usage{
ars_toc_template(path, overwrite = FALSE)
}
\arguments{
\item{path}{File path of the workbook to write (\code{.xlsx}).}

\item{overwrite}{Logical; overwrite an existing file? Default \code{FALSE}.}
}
\value{
The path, invisibly.
}
\description{
Creates the TOC workbook that drives the TOC-centred ARS chain (see
\code{\link{ars_from_toc}}): one row per planned display, referencing a
\strong{display recipe} (the bundled catalog of standard safety displays
transcribed from the FDA Standard Safety Tables & Figures Integrated Guide
and the PHUSE Analyses & Displays white papers) or \code{"custom"} for
hand-authored analyses.  The template is pre-filled with the standard safety
set and one custom efficacy example.
}
\section{Numbering}{
\code{toc_no} may be left blank: \code{\link{ars_from_toc}} assigns
\code{<section prefix>.<sequence>} numbers from each display's section key
using an ICH E3 section-14-style default map (baseline = 14.1,
efficacy = 14.2, adverse events = 14.3.1, labs = 14.3.4).  Real numbering
conventions are sponsor-specific -- normally assigned by biostatistics when
the TFL specification is created, anchored to the company CSR template
rather than inherited from the SAP -- so explicit \code{toc_no} values
always win and the section map is overridable (\code{section_map} argument
of \code{\link{ars_from_toc}}).
}
\examples{
tmp <- file.path(tempdir(), "toc.xlsx")
ars_toc_template(tmp, overwrite = TRUE)
readxl::read_excel(tmp, sheet = "TOC")
}
\seealso{
\code{\link{ars_from_toc}}; the recipe catalog README in
\code{system.file("ars-display-recipes", package = "ydisctools")}.
}
