% Generated manually for ydisctools (hand-maintained; keep in sync with the
% roxygen block in R/ars_tools.R).
\name{write_ars_xlsx}
\alias{write_ars_xlsx}
\title{Write ARS metadata tables to a siera-readable workbook}
\usage{
write_ars_xlsx(ars, path, overwrite = FALSE)
}
\arguments{
\item{ars}{Named list of data frames as returned by \code{\link{build_ars}}.}

\item{path}{File path of the workbook to write (\code{.xlsx}).}

\item{overwrite}{Logical; overwrite an existing file? Default \code{FALSE}.}
}
\value{
The path, invisibly.
}
\description{
Writes the list of ARS sheet tables produced by \code{\link{build_ars}} to
an \code{.xlsx} workbook that \code{siera::readARS()} accepts directly.  For
a JSON ARS, convert the written workbook with
\code{siera::ars_xlsx_to_json()} (available in siera > 0.5.6).
}
\examples{
tmp_params <- file.path(tempdir(), "ars_params.xlsx")
ars_param_template(tmp_params, overwrite = TRUE)
tmp_ars <- file.path(tempdir(), "ARS_STUDY01.xlsx")
write_ars_xlsx(build_ars(tmp_params), tmp_ars, overwrite = TRUE)
\dontrun{
siera::readARS(tmp_ars, output_path = tempdir(), adam_path = "path/to/adam")
}
}
\seealso{
\code{\link{build_ars}}, \code{\link{ars_param_template}}
}
