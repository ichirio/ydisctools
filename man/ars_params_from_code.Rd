% Generated manually for ydisctools (hand-maintained; keep in sync with the
% roxygen block in R/ars_from_code.R).
\name{ars_params_from_code}
\alias{ars_params_from_code}
\title{Recover compact ARS parameters from R programmes}
\usage{
ars_params_from_code(paths, output_id = NULL, output_name = NULL)
}
\arguments{
\item{paths}{Character vector of R source files (one output per file).}

\item{output_id}{Optional character vector (same length as \code{paths}) of
output IDs; default derives them from the file names.}

\item{output_name}{Optional character vector of output display names;
default uses the file names.}
}
\value{
A named list with elements \code{outputs}, \code{analyses},
\code{displays} (may have zero rows) and \code{notes} (character) -- the
first three in the compact parameter layout of \code{\link{ars_param_template}}.
}
\description{
Statically analyses one or more R programmes (no code is executed) and
drafts the compact parameter set for \code{\link{build_ars}}: analyses are
recovered from \pkg{cards} / \pkg{cardx} calls, populations / subsets /
group levels from the \code{dplyr::filter()} pipeline feeding them, and
display furniture (titles, footnotes, page headers / footers, the row-label
column header) from \pkg{rtfreporter} idioms where present.  Each input
file becomes one output (display).
}
\details{
Supported analysis calls: \code{ard_summary()} / \code{ard_continuous()},
\code{ard_tabulate()} / \code{ard_categorical()} (including the
siera-generated subject-count and big-N idioms), \code{ard_stack()}
(\code{.by}, \code{.total_n}, \code{.by_stats}),
\code{ard_stack_hierarchical()} (including
\code{over_variables = TRUE}; a nested hierarchy is flattened with a
LIMITATION note), and the cardx tests
\code{ard_stats_fisher_test/chisq_test/aov/anova/oneway_test/prop_test()}
(mapped onto the bundled method catalog).  Anything else is skipped with a
note.

Variable lists are resolved through the indirections real programmes use
(issue #32): a character vector assigned earlier in the file
(\code{vars <- c("AGE", ...)}) referenced directly or via
\code{all_of(vars)} / \code{any_of(vars)}, single names injected with
\code{!!sym(col)} / \code{syms()} or \code{.data[[col]]}, and
\pkg{magrittr} pipes (\code{df \%>\% ard_stack(...)}) are collapsed so the
piped data keeps feeding the dataset / population detection.  A wrapper
whose symbol has no literal assignment in the file keeps the symbol name
and is flagged with a REVIEW note.  When only the ARD itself is available,
see \code{\link{ars_params_from_ard}}.

The result is a DRAFT: review the \code{notes} element (and the
\code{Notes} sheet written by \code{\link{write_ars_params}}) before
building an ARS from it.
}
\examples{
# Recover parameters from a shipped siera-generated ARD programme:
src <- file.path(system.file("ars-examples", package = "ydisctools"),
                 "ARD_Out_dm.R")
params <- ars_params_from_code(src, output_id = "Out_dm",
                               output_name = "Summary of Demographic Data")
params$analyses
params$notes
}
\seealso{
\code{\link{write_ars_params}} to save the draft as a parameter workbook,
\code{\link{build_ars}} to expand it into ARS metadata.
}
