% Generated manually for ydisctools (hand-maintained; keep in sync with the
% roxygen block in R/ars_mock.R).
\name{ars_mock}
\alias{ars_mock}
\title{Generate TFL mock shells from a TOC workbook}
\usage{
ars_mock(
  toc,
  path,
  format = c("xlsx", "rtf"),
  section_map = NULL,
  overwrite = FALSE
)
}
\arguments{
\item{toc}{TOC workbook path or list, as for \code{\link{ars_from_toc}}.}

\item{path}{Output file path (\code{.xlsx} or \code{.rtf}, matching
\code{format}).}

\item{format}{\code{"xlsx"} (default) or \code{"rtf"}.}

\item{section_map}{Passed to \code{\link{ars_from_toc}} (numbering).}

\item{overwrite}{Logical; overwrite an existing file? Default \code{FALSE}.}
}
\value{
The path, invisibly.
}
\description{
Builds mock (shell) displays from the same TOC workbook that
\code{\link{ars_from_toc}} expands into ARS metadata -- the shells and the
ARS are derived from the same analyses, so they cannot drift apart.  Each
display gets a title block (\code{"Table <toc_no>"}, title, population
label), arm columns with \code{(N=XXX)} placeholders, and stub rows per
analysis: continuous methods emit the n / Mean (SD) / Median / Q1, Q3 /
Min, Max block, categorical methods emit indented category placeholder rows,
and \code{total_n} feeds the header \code{N=XXX} only.
}
\details{
Two formats:
\describe{
  \item{\code{"xlsx"}}{one sheet per display, written with \pkg{openxlsx}
    (title rows + bold column header).}
  \item{\code{"rtf"}}{ONE document with one display per page, written with
    \pkg{rtfreporter} -- the mock carries the same furniture the final
    rtfreporter table will.}
}
Both packages are optional (Suggests) and checked at run time.
}
\examples{
tmp_toc <- file.path(tempdir(), "toc.xlsx")
ars_toc_template(tmp_toc, overwrite = TRUE)
if (requireNamespace("openxlsx", quietly = TRUE)) {
  mock <- file.path(tempdir(), "mocks.xlsx")
  ars_mock(tmp_toc, mock, overwrite = TRUE)
}
}
\seealso{
\code{\link{ars_toc_template}}, \code{\link{ars_from_toc}}
}
