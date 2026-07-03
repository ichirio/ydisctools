% Generated manually for ydisctools (hand-maintained; keep in sync with the
% roxygen block in R/ars_tools.R).
\name{ars_param_template}
\alias{ars_param_template}
\title{Write a compact ARS parameter workbook template}
\usage{
ars_param_template(path, overwrite = FALSE)
}
\arguments{
\item{path}{File path of the workbook to write (\code{.xlsx}).}

\item{overwrite}{Logical; overwrite an existing file? Default \code{FALSE}.}
}
\value{
The path, invisibly.
}
\description{
Creates the compact parameter workbook consumed by \code{\link{build_ars}},
pre-filled with a worked example (a demographics summary and an
adverse-event summary on CDISC-pilot-style ADaM data).  Copy the file per
study, replace the example rows, and feed it back through
\code{\link{read_ars_params}} / \code{\link{build_ars}} /
\code{\link{write_ars_xlsx}} to obtain a full ARS workbook for
\code{siera::readARS()}.
}
\details{
The workbook has three sheets:
\describe{
  \item{\code{Study}}{key/value pairs (\code{study_id}, \code{study_title});
    used for workbook provenance only.}
  \item{\code{Outputs}}{one row per output (display): \code{output_id},
    \code{name}, and optional defaults \code{population}, \code{group_by},
    \code{groups} inherited by the output's analyses.}
  \item{\code{Analyses}}{one row per analysis: \code{output_id},
    \code{method} (a key of the bundled method catalog, e.g.
    \code{"total_n"}, \code{"categorical_summary"},
    \code{"continuous_summary"}), \code{dataset}, \code{variable}, and
    optional \code{analysis_id}, \code{name}, \code{population},
    \code{group_by}, \code{groups}, \code{groups2}, \code{where},
    \code{denominator}.}
}

Conditions (\code{population}, \code{where}) use a mini syntax: a bare flag
name (\code{"SAFFL"} means \code{ADSL.SAFFL EQ Y}), or
\code{"[DATASET.]VARIABLE COMPARATOR VALUE"} with comparators
\code{EQ, NE, GT, GE, LT, LE, IN, NOTIN, CONTAINS}; chain several with
\code{";"} (combined with AND); \code{IN}/\code{NOTIN} values are
pipe-separated (\code{"SEX IN M|F"}).

Note the ARS idiom for "n (\%) of subjects per category": the analysis
variable stays \code{USUBJID} and the category variable (\code{AGEGR1},
\code{SEX}, \code{AEBODSYS}, ...) enters as the \emph{second} grouping in
\code{group_by}.
}
\examples{
tmp <- file.path(tempdir(), "ars_params.xlsx")
ars_param_template(tmp, overwrite = TRUE)
params <- read_ars_params(tmp)
names(params)
}
\seealso{
\code{\link{read_ars_params}}, \code{\link{build_ars}},
\code{\link{write_ars_xlsx}}
}
