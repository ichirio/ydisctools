% Generated manually for ydisctools (hand-maintained; keep in sync with the
% roxygen block in R/ars_tools.R).
\name{build_ars}
\alias{build_ars}
\title{Build ARS metadata tables from compact parameters}
\usage{
build_ars(params)
}
\arguments{
\item{params}{Either the path to a compact parameter workbook (read via
\code{\link{read_ars_params}}) or a named list with elements \code{outputs}
and \code{analyses} (and optionally \code{study}) as returned by
\code{\link{read_ars_params}}.}
}
\value{
A named list of data frames, one per ARS workbook sheet, ready for
\code{\link{write_ars_xlsx}}.
}
\description{
Expands a compact parameter set (one row per analysis; see
\code{\link{ars_param_template}} for the format) into the full set of CDISC
Analysis Results Standard metadata tables required by
\code{siera::readARS()}: contents lists, analysis sets, groupings, data
subsets, analyses, and the analysis-method sheets (expanded from the bundled
siera method catalog).  Repeated structures are de-duplicated and IDs are
assigned automatically; numerator/denominator wiring for percentage-based
methods is resolved from the \code{denominator} column (\code{"auto"} finds
the \code{total_n} analysis with the same population and first grouping in
the same output).  An ARS \code{Outputs} sheet is always emitted; when the
parameters carry display metadata (a \code{Displays} sheet / element, see
\code{\link{ars_param_template}}), the ARS \code{Displays} and
\code{GlobalDisplaySections} sheets are emitted as well.
}
\section{Sizing note}{
\code{siera::readARS()} (as of siera 0.5.x) indexes the third analysis of
each output while deciding how to build the population filter, so
\code{build_ars()} requires at least three analyses per output.
}
\section{Worked example set}{
\code{system.file("ars-examples", package = "ydisctools")} ships the full
artefact chain for a Demographics and an Adverse Events display: the compact
parameter workbook, the ARS workbook built from it, the ARD programmes
\code{siera::readARS()} generated, and dummy ADaM data the programmes run on
(see the README there).
}
\examples{
tmp <- file.path(tempdir(), "ars_params.xlsx")
ars_param_template(tmp, overwrite = TRUE)
ars <- build_ars(tmp)
names(ars)
ars$Analyses
}
\seealso{
\code{\link{ars_param_template}} for the parameter format,
\code{\link{write_ars_xlsx}} to write the result, and
\code{siera::ars_xlsx_to_json()} (siera > 0.5.6) to convert the written
workbook to the JSON ARS representation.
}
