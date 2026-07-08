% Generated manually for ydisctools (hand-maintained; keep in sync with the
% roxygen block in R/ars_run.R).
\name{ars_generate_ard}
\alias{ars_generate_ard}
\title{Generate and run ARD programmes, returning one combined ARD}
\usage{
ars_generate_ard(
  ars,
  adam_path = if (run) NULL else "adam",
  output_path = tempfile("ars_ard_"),
  spec_output = "",
  combine = TRUE,
  run = TRUE
)
}
\arguments{
\item{ars}{Path to an ARS metadata file (\code{.xlsx} or \code{.json}), or
the named list of ARS sheets returned by \code{\link{build_ars}} (written to
a temporary workbook automatically).}

\item{adam_path}{Folder the generated programmes read the ADaM datasets
from, in any format \code{\link{read_adam}} supports (\code{.xpt} /
\code{.sas7bdat} / \code{.rds} / \code{.rda} / \code{.csv}).  Required when
\code{run = TRUE} (and must exist); optional when \code{run = FALSE}, where
it only sets the path baked into the code (default \code{"adam"}).}

\item{output_path}{Folder to write the generated \code{ARD_<OutputId>.R}
programmes to. Default: a fresh temporary directory.  The programmes are
kept there for inspection / traceability.}

\item{spec_output}{Run only this output ID (passed to
\code{siera::readARS()}). Default \code{""} runs every output.}

\item{combine}{Logical; \code{TRUE} (default) returns one combined data
frame, \code{FALSE} a named list with one ARD per output.  Ignored when
\code{run = FALSE}.}

\item{run}{Logical; \code{TRUE} (default) runs the programmes and returns
the ARD(s).  \code{FALSE} only generates the programmes (no data needed)
and returns their file paths invisibly.}
}
\value{
When \code{run = TRUE}: a data frame (the combined ARD) if
\code{combine = TRUE}, else a named list of per-output ARD data frames; the
generated programme paths are attached as attribute \code{"scripts"}.  When
\code{run = FALSE}: the character vector of generated programme paths,
invisibly.
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

\strong{Reading the ADaM data.}  siera 0.5.6 hardcodes
\code{readr::read_csv('<adam_path>/<DATASET>.csv')} in every programme.
This wrapper rewrites that reader to \code{\link{read_adam}}, so the actual
file present in \code{adam_path} is read by format priority (\code{.xpt} >
\code{.sas7bdat} > \code{.rds} > \code{.rda} > \code{.csv}) -- real
transport / SAS / R datasets work without a manual conversion to csv.

\strong{Generating without data.}  siera bakes only a \emph{path} into the
code at generation time; the data are read when the programmes \emph{run}.
With \code{run = FALSE} the programmes are generated and the reader is
rewritten, but nothing is executed and \code{adam_path} need not contain
(or be) any data -- use it to obtain the R code alone.
}
\examples{
\dontrun{
ex <- system.file("ars-examples", package = "ydisctools")
ard <- ars_generate_ard(file.path(ex, "ARS_dm_ae.xlsx"),
                        adam_path = file.path(ex, "adam"))
table(ard$OutputId)

# code only, no data required:
scripts <- ars_generate_ard(file.path(ex, "ARS_dm_ae.xlsx"),
                            run = FALSE)
}
}
\seealso{
\code{\link{read_adam}}, \code{\link{build_ars}}, \code{\link{write_ars_xlsx}}
}
