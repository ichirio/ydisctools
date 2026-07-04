% Generated manually for ydisctools (hand-maintained; keep in sync with the
% roxygen block in R/ars_run.R).
\name{ars_generate_ard}
\alias{ars_generate_ard}
\title{Generate and run ARD programmes, returning one combined ARD}
\usage{
ars_generate_ard(
  ars,
  adam_path,
  output_path = tempfile("ars_ard_"),
  spec_output = "",
  combine = TRUE
)
}
\arguments{
\item{ars}{Path to an ARS metadata file (\code{.xlsx} or \code{.json}), or
the named list of ARS sheets returned by \code{\link{build_ars}} (written to
a temporary workbook automatically).}

\item{adam_path}{Folder containing the ADaM datasets (\code{.csv} /
\code{.xpt}) the programmes read.}

\item{output_path}{Folder to write the generated \code{ARD_<OutputId>.R}
programmes to. Default: a fresh temporary directory.  The programmes are
kept there for inspection / traceability.}

\item{spec_output}{Run only this output ID (passed to
\code{siera::readARS()}). Default \code{""} runs every output.}

\item{combine}{Logical; \code{TRUE} (default) returns one combined data
frame, \code{FALSE} a named list with one ARD per output.}
}
\value{
A data frame (the combined ARD) when \code{combine = TRUE}, otherwise a
named list of per-output ARD data frames.  The paths of the generated
programmes are attached as attribute \code{"scripts"}.
}
\description{
A thin driver around \code{siera::readARS()}: generates the per-output ARD
programmes from an ARS metadata file (or directly from the sheet list
returned by \code{\link{build_ars}}), runs each programme in an isolated
environment, and returns the Analysis Results Datasets bound into a single
data frame.  siera stamps \code{AnalysisId} / \code{MethodId} /
\code{OutputId} onto every ARD row, so the combined ARD is keyed by output
out of the box.
}
\details{
The generated programmes open with \code{library()} calls for siera's
runtime stack; because every generated statement is namespace-qualified,
those lines are neutralised before sourcing, so running the programmes does
not attach packages to the caller's search path.  The packages the generated
code actually calls (\pkg{cards}, \pkg{dplyr}, \pkg{readr}, and \pkg{cardx}
for test methods) must be installed.
}
\examples{
\dontrun{
ex <- system.file("ars-examples", package = "ydisctools")
ard <- ars_generate_ard(file.path(ex, "ARS_dm_ae.xlsx"),
                        adam_path = file.path(ex, "adam"))
table(ard$OutputId)
}
}
\seealso{
\code{\link{build_ars}}, \code{\link{write_ars_xlsx}}
}
