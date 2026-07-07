% Generated manually for ydisctools (hand-maintained; keep in sync with the
% roxygen block in R/ars_recover.R).
\name{ars_params_recover}
\alias{ars_params_recover}
\title{Recover compact ARS parameters from a programme, an ARD, or both}
\usage{
ars_params_recover(paths = NULL, ard = NULL, output_id = NULL,
  output_name = NULL, dataset = "UNKNOWN")
}
\arguments{
\item{paths}{Optional character vector of R source files, as in
\code{\link{ars_params_from_code}}.}

\item{ard}{Optional ARD data frame or flattened \code{.csv} / \code{.xlsx}
path, as in \code{\link{ars_params_from_ard}}.}

\item{output_id, output_name}{Optional character vectors of output IDs /
display names (one per output), applied to both sources.}

\item{dataset}{Fallback dataset name for ARD-only analyses when the code
provides none. Default \code{"UNKNOWN"}.}
}
\value{
A named list with \code{outputs}, \code{analyses}, \code{displays} and
\code{notes}, as for \code{\link{ars_params_from_code}} -- ready for
\code{\link{write_ars_params}} / \code{\link{build_ars}}.  A DRAFT: review
the notes.
}
\description{
The recommended front door of the parameter-recovery family.  With only
\code{paths} it delegates to \code{\link{ars_params_from_code}}; with only
\code{ard} to \code{\link{ars_params_from_ard}}.  With \strong{both}, the
two drafts are merged for a more accurate result than either source alone:
}
\details{
\itemize{
\item the \strong{ARD is authoritative for which analyses exist} -- every
analysis unit found in the ARD is kept (the code may fail to resolve a
variable list, or not parse at all);
\item each ARD-derived analysis is matched to a code-derived analysis by
its (method, variable, groupings) signature -- order-preserving within
duplicate signatures, so e.g. two flat AE analyses that differ only in
their \code{where} subset pair up in appearance order -- and enriched with
the code's \code{population}, \code{where}, \code{dataset}, \code{groups}
and analysis name;
\item ARD analyses with no code match inherit the output's unanimous
code-side population / dataset (ASSUMED note); analyses found only in the
code (never ran, or conditional) are reported as discrepancy notes -- the
cross-check itself is part of the value;
\item display furniture (\code{displays}) always comes from the code, and
the outputs' \code{groups} prefer the code's pre-defined levels over the
ARD's observed ones.
}

Outputs are paired positionally (\code{paths[i]} with the i-th
\code{OutputId} group of the ARD), so supply one programme per ARD output;
\code{output_id} overrides the IDs of both sides.

When both sources recovered analyses for an output but \strong{not one
pair} agrees on the (method, variable, groupings) signature, the merge
stops with an error: the programme and the ARD describe different
displays, which usually means a mismatched pairing (e.g. a stale
\code{ard} object left over from another display).  If the pairing is
intentional, recover each source on its own instead.
}
\examples{
# a programme whose variable list cannot be resolved from this file ...
src <- file.path(tempdir(), "t_dm.R")
writeLines('
  library(cards); library(dplyr)
  adsl <- ADSL |> filter(SAFFL == "Y")
  ard <- ard_stack(adsl, .by = TRT01A,
                   ard_summary(variables = all_of(vars_from_elsewhere)),
                   .total_n = TRUE)', src)
# ... plus the ARD it produced (which knows the variables that ran)
ard <- data.frame(
  group1 = "TRT01A", group1_level = rep(c("Placebo", "Active"), 3),
  variable = rep(c("TRT01A", "AGE", "HTBL"), each = 2),
  variable_level = NA, stat_name = rep(c("n", "mean", "mean"), each = 2),
  stat = c(10, 10, 70.1, 71.2, 165.3, 166.0)
)
ard$group1[1:2] <- NA; ard$group1_level[1:2] <- NA
ard$variable_level[1:2] <- c("Placebo", "Active")

rec <- ars_params_recover(src, ard, output_id = "Out_dm")
# AGE and HTBL from the ARD; SAFFL population and ADSL from the code
rec$analyses[, c("name", "method", "variable", "dataset", "population")]
}
\seealso{
\code{\link{ars_params_from_code}}, \code{\link{ars_params_from_ard}},
\code{\link{write_ars_params}}, \code{\link{build_ars}}
}
