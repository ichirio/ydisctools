% Generated manually for ydisctools (hand-maintained; keep in sync with the
% roxygen block in R/ars_from_ard.R).
\name{ars_params_from_ard}
\alias{ars_params_from_ard}
\title{Recover compact ARS parameters from an ARD}
\usage{
ars_params_from_ard(ard, output_id = NULL, output_name = NULL,
  dataset = "UNKNOWN")
}
\arguments{
\item{ard}{An ARD data frame, or the path of a flattened ARD file
(\code{.csv} or \code{.xlsx}).  Must carry at least the \code{variable}
and \code{stat_name} columns; \code{group1} / \code{group1_level} (etc.),
\code{variable_level}, \code{context}, \code{OutputId} /
\code{AnalysisId} / \code{MethodId} are used when present.}

\item{output_id}{Optional character vector of output IDs.  Default: the
ARD's \code{OutputId} values (one output per ID), or \code{"Out_ard"}
when the column is absent.}

\item{output_name}{Optional character vector of output display names (same
length as the outputs); default uses the output IDs.}

\item{dataset}{Dataset name to record on every analysis (an ARD does not
carry it). Default \code{"UNKNOWN"}.}
}
\value{
A named list with elements \code{outputs}, \code{analyses},
\code{displays} (\code{NULL}; an ARD carries no display furniture) and
\code{notes} -- the same shape as \code{\link{ars_params_from_code}}, so
\code{\link{write_ars_params}} and \code{\link{build_ars}} accept it
unchanged.
}
\description{
Drafts the compact parameter set for \code{\link{build_ars}} from an
existing \strong{Analysis Results Dataset} -- a \pkg{cards} ARD
(\code{ard_stack()} output, possibly \code{unlist_ard_columns()}-flattened),
a siera-generated combined ARD (as returned by
\code{\link{ars_generate_ard}}), or a flattened csv/xlsx export of either.
Use this when the programme that produced the ARD is not available or
cannot be read by \code{\link{ars_params_from_code}}.
}
\details{
Rows are grouped into analyses -- by \code{AnalysisId} when the ARD is
siera-stamped, otherwise by the grouping-columns/variable combination --
and each analysis is classified onto the bundled method catalog:

\itemize{
\item continuous statistics (\code{mean}, \code{sd}, \code{median},
\code{p25}, ...) become \code{continuous_summary} analyses of the ARD
\code{variable};
\item \code{n} / \code{p} tabulations become \code{categorical_summary}
analyses in the ARS idiom (\code{variable = "USUBJID"}, the category
variable as the \emph{second} grouping, \code{denominator = "auto"});
\item proportion-CI stat sets (\code{conf.low} / \code{conf.high}, cardx
context \code{proportion_ci}) become \code{proportion_ci} analyses, with
the confidence level (and CI method, when the ARD kept those rows)
recovered onto \code{options};
\item the subject-count idioms become \code{total_n}: cards'
\code{..total_n..} rows, an ungrouped tabulation of a variable that other
analyses use as their grouping (e.g. \code{.by_stats = TRUE} output), and
the ydisctools \code{.flag_} pattern maps to a flat per-group count;
\item \code{ard_stack_hierarchical()} ARDs are flattened the same way
\code{\link{ars_params_from_code}} flattens the call: the
\code{over_variables} rows become \emph{Subjects with at least one event,
n (\%)}, deeper levels drop their hierarchy parents from the groupings
(LIMITATION note), and cards sentinel markers
(\code{..ard_hierarchical_overall..} etc.) never leak into
\code{variable} / \code{group_by} (issue #46) -- an unrecognised sentinel
is skipped with a REVIEW note.
}

An ARD carries no data provenance, so \code{dataset} (unless supplied)
and \code{where} conditions cannot be recovered and are surfaced as REVIEW
notes.  \code{population} is set to \code{"ALL"} on the output row: the
ARD's numbers were computed from the analysed data as-is, so "no further
analysis-set filter" is the faithful recovery -- an ASSUMED note reminds
you to change it to a flag condition (e.g. \code{SAFFL}) if the built ARS
will run on unfiltered source data.  The observed levels of the first
grouping are emitted as the pre-defined \code{groups} with an ASSUMED
note.  Like \code{\link{ars_params_from_code}}, the result is a DRAFT to
review.
}
\examples{
# a minimal flattened ARD: per-arm N, Age summary, Sex n (\%)
ard <- data.frame(
  group1         = c(rep(NA, 2), rep("TRT01A", 8)),
  group1_level   = c(rep(NA, 2), rep(c("Placebo", "Active"), 4)),
  variable       = c(rep("TRT01A", 2), rep("AGE", 4), rep("SEX", 4)),
  variable_level = c("Placebo", "Active", rep(NA, 4), "F", "F", "M", "M"),
  stat_name      = c("n", "n", "mean", "mean", "sd", "sd",
                     "n", "n", "p", "p"),
  stat           = c(10, 10, 70.1, 71.2, 8.1, 7.9, 6, 5, 0.6, 0.5)
)
rec <- ars_params_from_ard(ard, output_id = "Out_dm", dataset = "ADSL")
rec$analyses[, c("analysis_id", "name", "method", "variable", "group_by")]
rec$notes
}
\seealso{
\code{\link{ars_params_from_code}} for recovery from the programme source,
\code{\link{ars_params_recover}} to merge both sources,
\code{\link{write_ars_params}}, \code{\link{build_ars}}
}
