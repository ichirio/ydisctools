% Generated manually for ydisctools (hand-maintained; keep in sync with the
% roxygen block in R/ars_toc.R).
\name{ars_from_toc}
\alias{ars_from_toc}
\title{Expand a TFL TOC workbook into compact ARS parameters}
\usage{
ars_from_toc(toc, section_map = NULL)
}
\arguments{
\item{toc}{Path to a TOC workbook, or a named list with elements \code{toc}
(the TOC data frame) and optionally \code{study}, \code{analyses} (custom
rows) and \code{displays} (extra display furniture, as in
\code{\link{ars_param_template}}'s \code{Displays} sheet).}

\item{section_map}{Named character vector mapping section keys to number
prefixes; default ICH E3-style \code{c(baseline = "14.1",
efficacy = "14.2", safety_ae = "14.3.1", safety_lab = "14.3.4")}.  Override
to follow a sponsor CSR template's numbering.}
}
\value{
A compact parameter list (\code{study}, \code{outputs}, \code{analyses},
\code{displays}, plus the resolved \code{toc}) ready for
\code{\link{build_ars}} / \code{\link{ars_generate_ard}} /
\code{\link{write_ars_params}}.
}
\description{
The TOC workbook (see \code{\link{ars_toc_template}}) is the hub of the
SAP -> TOC -> mock -> ARS chain: one row per planned display.  Rows with a
catalogued \code{display_type} are expanded through the bundled display
recipes (the TOC row supplies the study specifics: population, arm grouping,
pre-defined arm levels, extra \code{where} conditions -- the extra
conditions narrow every analysis of the display except \code{total_n}
big-N denominators, which stay at analysis-set level); rows with
\code{display_type = "custom"} take their hand-authored analyses from the
workbook's \code{Analyses} sheet.  Missing \code{toc_no} values are
auto-numbered from the section map (see \code{\link{ars_toc_template}}'s
Numbering section), and each display's number and title become ARS
\code{Displays} Title subsections (\code{"Table <toc_no>"} + title).
}
\examples{
tmp <- file.path(tempdir(), "toc.xlsx")
ars_toc_template(tmp, overwrite = TRUE)
params <- ars_from_toc(tmp)
params$toc[, c("toc_no", "output_id", "display_type")]
head(params$analyses[, c("output_id", "analysis_id", "method", "group_by")])
}
\seealso{
\code{\link{ars_toc_template}}, \code{\link{build_ars}},
\code{\link{ars_generate_ard}}
}
