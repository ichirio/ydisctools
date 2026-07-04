% Generated manually for ydisctools (hand-maintained; keep in sync with the
% roxygen block in R/ars_sap.R).
\name{read_csr_map}
\alias{read_csr_map}
\title{Derive a TOC numbering section map from a CSR template}
\usage{
read_csr_map(
  docx,
  keywords = NULL,
  anchor_keyword = "tables?, figures?|figures? and graphs|listing"
)
}
\arguments{
\item{docx}{Path to the CSR template (\code{.docx}).}

\item{keywords}{Named list of regular expressions (matched
case-insensitively against heading titles) defining the section keys;
default maps \code{baseline} / \code{efficacy} / \code{safety_ae} /
\code{safety_lab}.}

\item{anchor_keyword}{Regular expression locating the tables-section anchor;
\code{NULL} disables anchoring.}
}
\value{
A list with \code{section_map} (named character vector for
\code{\link{ars_from_toc}}), \code{headings} (the parsed heading table with
a \code{reconstructed} flag) and \code{notes} (review notes).
}
\description{
Reads a (company) CSR template or shell document, walks its heading
structure, and derives the \code{section_map} that \code{\link{ars_from_toc}}
/ \code{\link{ars_mock}} use for rule-based \code{toc_no} numbering -- so
display numbers follow the company's CSR section structure instead of the
ICH E3 defaults.
}
\details{
Literal section numbers are taken from the heading text when present.  When
the template uses Word automatic numbering (no number in the text), numbers
are \emph{reconstructed} rule-based from the heading levels in document
order -- review the \code{headings} element in that case.  When a heading
matching \code{anchor_keyword} (default: the ICH E3 "tables, figures and
graphs" section) exists, keyword matching is restricted to its subtree, so
body sections (e.g. a section 12 "Adverse Events" discussion) do not capture
the map.
}
\examples{
if (requireNamespace("officer", quietly = TRUE)) {
# build a tiny CSR-like template
doc <- officer::read_docx()
doc <- officer::body_add_par(doc, "12 Safety Evaluation", style = "heading 1")
doc <- officer::body_add_par(doc, "14 Tables, Figures and Graphs",
                             style = "heading 1")
doc <- officer::body_add_par(doc, "14.1 Demographic Data", style = "heading 2")
doc <- officer::body_add_par(doc, "14.2 Efficacy Data", style = "heading 2")
doc <- officer::body_add_par(doc, "14.3 Adverse Events", style = "heading 2")
tmp <- file.path(tempdir(), "csr.docx")
print(doc, target = tmp)
read_csr_map(tmp)$section_map
}
}
\seealso{
\code{\link{ars_from_toc}}, \code{\link{read_sap_toc}}
}
