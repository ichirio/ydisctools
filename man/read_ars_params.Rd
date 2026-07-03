% Generated manually for ydisctools (hand-maintained; keep in sync with the
% roxygen block in R/ars_tools.R).
\name{read_ars_params}
\alias{read_ars_params}
\title{Read a compact ARS parameter workbook}
\usage{
read_ars_params(path)
}
\arguments{
\item{path}{Path to the compact parameter workbook (\code{.xlsx}).}
}
\value{
A named list with elements \code{study} (may be \code{NULL}),
\code{outputs} and \code{analyses}.
}
\description{
Reads a workbook in the format written by \code{\link{ars_param_template}}
back into a named list of data frames suitable for \code{\link{build_ars}}.
}
\examples{
tmp <- file.path(tempdir(), "ars_params.xlsx")
ars_param_template(tmp, overwrite = TRUE)
params <- read_ars_params(tmp)
params$outputs
}
\seealso{
\code{\link{ars_param_template}}, \code{\link{build_ars}}
}
