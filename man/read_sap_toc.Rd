% Generated manually for ydisctools (hand-maintained; keep in sync with the
% roxygen block in R/ars_sap.R).
\name{read_sap_toc}
\alias{read_sap_toc}
\title{Draft a TFL TOC from a SAP Word document}
\usage{
read_sap_toc(docx)
}
\arguments{
\item{docx}{Path to the SAP (\code{.docx}).}
}
\value{
A list with \code{toc} (draft TOC rows: \code{toc_no}, \code{output_id},
\code{title}, \code{display_type}, \code{section_key}, \code{population},
\code{group_by}, \code{groups}, \code{where}) and \code{notes}.
}
\description{
Statically reads a SAP (\code{.docx}, e.g. based on the TransCelerate
template) and drafts TOC workbook rows from it: tables that look like a
planned-display list (a header row with number- and title-like columns)
become TOC rows, display numbers are cleaned (\code{"Table 14.1.1"} ->
\code{"14.1.1"}), and each title is keyword-matched against the bundled
display-recipe catalog (unmatched displays stay \code{"custom"}).
}
\details{
This is a DRAFT generator: review the \code{notes}, fill in
\code{population} / \code{group_by} (and \code{Analyses} rows for custom
displays), then feed the result to \code{\link{ars_from_toc}}.  SAPs that
carry no planned-display table yield an empty draft -- numbering conventions
normally live in the TFL specification rather than the SAP, which is why the
TOC workbook (not the SAP) is the chain's source of truth.
}
\examples{
if (requireNamespace("officer", quietly = TRUE)) {
# build a tiny SAP-like document with a planned-display table
displays <- data.frame(
  `Table No.` = c("14.1.1", "14.3.1.1"),
  Title = c("Summary of Demographic Data",
            "Overall Summary of Treatment-Emergent Adverse Events"),
  Population = c("Safety", "Safety"),
  check.names = FALSE
)
doc <- officer::read_docx()
doc <- officer::body_add_par(doc, "Planned displays", style = "heading 1")
doc <- officer::body_add_table(doc, displays)
tmp <- file.path(tempdir(), "sap.docx")
print(doc, target = tmp)
read_sap_toc(tmp)$toc
}
}
\seealso{
\code{\link{ars_from_toc}}, \code{\link{read_csr_map}}
}
