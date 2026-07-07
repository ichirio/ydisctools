% Generated manually for ydisctools (hand-maintained; keep in sync with the
% roxygen block in R/ars_from_code.R).
\name{write_ars_params}
\alias{write_ars_params}
\title{Write a recovered parameter set to a compact parameter workbook}
\usage{
write_ars_params(params, path, overwrite = FALSE)
}
\arguments{
\item{params}{Named list with \code{outputs} and \code{analyses} data
frames; optional \code{study}, \code{displays} and \code{notes}.}

\item{path}{File path of the workbook to write (\code{.xlsx}).}

\item{overwrite}{Logical; overwrite an existing file? Default \code{FALSE}.}
}
\value{
The path, invisibly.
}
\description{
Saves the result of \code{\link{ars_params_from_code}} (or any compatible
list with \code{outputs} / \code{analyses}, optionally \code{displays} and
\code{notes}) as a workbook in the \code{\link{ars_param_template}} layout,
with the review notes on an extra \code{Notes} sheet.  The file can be
edited and read back with \code{\link{read_ars_params}}, then expanded with
\code{\link{build_ars}}.

When the draft is missing a value \code{\link{build_ars}} requires -- a
\code{population} or \code{group_by} with neither an analysis-row value
nor an output-row default -- the file is still written (it is a draft),
but a warning lists the gaps immediately instead of leaving them to
surface as a later \code{\link{build_ars}} error.
}
\examples{
src <- file.path(system.file("ars-examples", package = "ydisctools"),
                 "ARD_Out_dm.R")
params <- ars_params_from_code(src, output_id = "Out_dm",
                               output_name = "Summary of Demographic Data")
tmp <- file.path(tempdir(), "recovered_params.xlsx")
write_ars_params(params, tmp, overwrite = TRUE)
}
\seealso{
\code{\link{ars_params_from_code}}, \code{\link{read_ars_params}},
\code{\link{build_ars}}
}
