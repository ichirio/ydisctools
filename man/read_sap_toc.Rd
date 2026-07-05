% Generated manually for ydisctools (hand-maintained; keep in sync with the
% roxygen block in R/ars_sap.R).
\name{read_sap_toc}
\alias{read_sap_toc}
\title{Draft a TFL TOC from a SAP Word document}
\usage{
read_sap_toc(docx, mode = c("auto", "table", "prose"))
}
\arguments{
\item{docx}{Path to the SAP (\code{.docx}).}

\item{mode}{Which extraction route to use: \code{"auto"} (default; try the
table route, fall through to the prose route), \code{"table"} or
\code{"prose"}.}
}
\value{
A list with \code{toc} (draft TOC rows: \code{toc_no}, \code{output_id},
\code{title}, \code{display_type}, \code{section_key}, \code{population},
\code{group_by}, \code{groups}, \code{where}, and \code{source} -- the SAP
sentence a prose row came from, \code{NA} for table rows) and \code{notes}.
}
\description{
Statically reads a SAP (\code{.docx}, e.g. based on the TransCelerate
template) and drafts TOC workbook rows from it via two routes:
\describe{
  \item{\strong{table route}}{a table that looks like a planned-display list
    (a header row with number- and title-like columns) becomes TOC rows;
    display numbers are cleaned (\code{"Table 14.1.1"} -> \code{"14.1.1"}).}
  \item{\strong{prose route}}{when no such table exists, the
    \dQuote{Statistical Methods / Analyses} section is scanned for display
    declarations -- sentences whose \emph{subject} names a clinical data
    domain and that carry a display verb (\dQuote{Demographic data
    \strong{will be summarized} by treatment group}).  Statistics-detail
    lines (\dQuote{95\% CIs will be provided}) name no domain and are dropped;
    the scan is anchored to that section's subtree so out-of-scope prose does
    not leak in.  The verbatim sentence is kept in the \code{source} column
    for review.}
}

Each title/sentence is keyword-matched against the bundled display-recipe
catalog (unmatched displays stay \code{"custom"}).  Real SAPs rarely carry a
planned-display table, so \code{"auto"} usually resolves to the prose route.
}
\details{
This is a DRAFT generator: review the \code{notes} and each row's
\code{source}, fill in \code{population} / \code{group_by} (and
\code{Analyses} rows for custom displays), then feed the result to
\code{\link{ars_from_toc}}.  Numbering conventions normally live in the TFL
specification rather than the SAP, which is why the TOC workbook (not the
SAP) remains the chain's source of truth.
}
\examples{
if (requireNamespace("officer", quietly = TRUE)) {
# a SAP whose display list lives in Statistical Methods prose (no table)
doc <- officer::read_docx()
doc <- officer::body_add_par(doc, "Statistical Methods", style = "heading 1")
doc <- officer::body_add_par(doc,
  "Demographic and baseline characteristics will be summarized by treatment group.")
doc <- officer::body_add_par(doc,
  "Treatment-emergent adverse events will be summarized by system organ class.")
tmp <- file.path(tempdir(), "sap.docx")
print(doc, target = tmp)
read_sap_toc(tmp)$toc
}
}
\seealso{
\code{\link{ars_from_toc}}, \code{\link{read_csr_map}}
}
