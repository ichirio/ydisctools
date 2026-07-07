# ============================================================================
#  ars_recover.R -- merge code-based and ARD-based parameter recovery
#  (issue #35, follow-up to #32)
# ============================================================================
#
#  The two single-source extractors are complementary:
#    * ars_params_from_code() knows the data PROVENANCE (dataset names,
#      population flags, `where` subsets, pre-defined group levels, display
#      furniture) but is blind to whatever it cannot parse or resolve;
#    * ars_params_from_ard() knows the definitive list of analyses that
#      actually RAN (variables, groupings, methods, observed levels) but an
#      ARD carries no provenance at all.
#  ars_params_recover() runs both and reconciles them: the ARD decides WHICH
#  analyses exist, the code fills in WHERE the data came from, and every
#  disagreement becomes an explicit note.
# ============================================================================

#' Recover compact ARS parameters from a programme, an ARD, or both
#'
#' The recommended front door of the parameter-recovery family.  With only
#' \code{paths} it delegates to [ars_params_from_code()]; with only
#' \code{ard} to [ars_params_from_ard()].  With \strong{both}, the two
#' drafts are merged for a more accurate result than either source alone:
#'
#' \itemize{
#'   \item the \strong{ARD is authoritative for which analyses exist} --
#'     every analysis unit found in the ARD is kept (the code may fail to
#'     resolve a variable list, or not parse at all);
#'   \item each ARD-derived analysis is matched to a code-derived analysis
#'     by its (method, variable, groupings) signature -- order-preserving
#'     within duplicate signatures, so e.g. two flat AE analyses that differ
#'     only in their \code{where} subset pair up in appearance order -- and
#'     enriched with the code's \code{population}, \code{where},
#'     \code{dataset}, \code{groups} and analysis name;
#'   \item ARD analyses with no code match inherit the output's unanimous
#'     code-side population / dataset (ASSUMED note); analyses found only in
#'     the code (never ran, or conditional) are reported as discrepancy
#'     notes -- the cross-check itself is part of the value;
#'   \item display furniture (\code{displays}) always comes from the code,
#'     and the outputs' \code{groups} prefer the code's pre-defined levels
#'     over the ARD's observed ones.
#' }
#'
#' Outputs are paired positionally (\code{paths[i]} with the i-th
#' \code{OutputId} group of the ARD), so supply one programme per ARD
#' output; \code{output_id} overrides the IDs of both sides.
#'
#' @param paths Optional character vector of R source files, as in
#'   [ars_params_from_code()].
#' @param ard Optional ARD data frame or flattened \code{.csv} /
#'   \code{.xlsx} path, as in [ars_params_from_ard()].
#' @param output_id,output_name Optional character vectors of output IDs /
#'   display names (one per output), applied to both sources.
#' @param dataset Fallback dataset name for ARD-only analyses when the code
#'   provides none. Default \code{"UNKNOWN"}.
#'
#' @return A named list with \code{outputs}, \code{analyses},
#'   \code{displays} and \code{notes}, as for [ars_params_from_code()] --
#'   ready for [write_ars_params()] / [build_ars()].  A DRAFT: review the
#'   notes.
#'
#' @seealso [ars_params_from_code()], [ars_params_from_ard()],
#'   [write_ars_params()], [build_ars()]
#'
#' @examples
#' # a programme whose variable list cannot be resolved from this file ...
#' src <- file.path(tempdir(), "t_dm.R")
#' writeLines('
#'   library(cards); library(dplyr)
#'   adsl <- ADSL |> filter(SAFFL == "Y")
#'   ard <- ard_stack(adsl, .by = TRT01A,
#'                    ard_summary(variables = all_of(vars_from_elsewhere)),
#'                    .total_n = TRUE)', src)
#' # ... plus the ARD it produced (which knows the variables that ran)
#' ard <- data.frame(
#'   group1 = "TRT01A", group1_level = rep(c("Placebo", "Active"), 3),
#'   variable = rep(c("TRT01A", "AGE", "HTBL"), each = 2),
#'   variable_level = NA, stat_name = rep(c("n", "mean", "mean"), each = 2),
#'   stat = c(10, 10, 70.1, 71.2, 165.3, 166.0)
#' )
#' ard$group1[1:2] <- NA; ard$group1_level[1:2] <- NA
#' ard$variable_level[1:2] <- c("Placebo", "Active")
#'
#' rec <- ars_params_recover(src, ard, output_id = "Out_dm")
#' # AGE and HTBL from the ARD; SAFFL population and ADSL from the code
#' rec$analyses[, c("name", "method", "variable", "dataset", "population")]
#'
#' @export
ars_params_recover <- function(paths = NULL, ard = NULL,
                               output_id = NULL, output_name = NULL,
                               dataset = "UNKNOWN") {
  if (is.null(paths) && is.null(ard)) {
    stop("Supply `paths` (programme files), `ard` (an ARD), or both.",
         call. = FALSE)
  }
  if (is.null(ard)) {
    return(ars_params_from_code(paths, output_id = output_id,
                                output_name = output_name))
  }
  if (is.null(paths)) {
    return(ars_params_from_ard(ard, output_id = output_id,
                               output_name = output_name,
                               dataset = dataset))
  }

  from_ard <- ars_params_from_ard(ard, output_id = output_id,
                                  output_name = output_name,
                                  dataset = dataset)
  n_out <- nrow(from_ard$outputs)
  if (length(paths) != n_out) {
    stop("`paths` must supply one programme per ARD output (ARD has ",
         n_out, " output(s), got ", length(paths), " file(s)).",
         call. = FALSE)
  }
  from_code <- ars_params_from_code(
    paths,
    output_id = from_ard$outputs$output_id,
    output_name = from_ard$outputs$name
  )

  notes <- unique(c(from_ard$notes, from_code$notes))
  note <- function(msg) notes <<- c(notes, msg)

  merged <- from_ard$analyses
  code_an <- from_code$analyses
  sig <- function(df) paste(df$method, df$variable, df$group_by, sep = "\r")

  for (oid in from_ard$outputs$output_id) {
    a_idx <- which(merged$output_id == oid)
    c_idx <- if (is.null(code_an)) integer(0) else
      which(code_an$output_id == oid)
    if (length(c_idx) == 0) next
    a_sig <- sig(merged[a_idx, , drop = FALSE])
    c_sig <- sig(code_an[c_idx, , drop = FALSE])
    c_used <- rep(FALSE, length(c_idx))

    for (k in seq_along(a_idx)) {
      j <- which(c_sig == a_sig[k] & !c_used)
      if (length(j) == 0) next
      j <- j[1]; c_used[j] <- TRUE
      src <- code_an[c_idx[j], ]
      for (col in c("population", "where", "dataset", "groups", "name",
                    "options")) {
        v <- src[[col]]
        if (!is.na(v) && nzchar(v)) merged[[col]][a_idx[k]] <- v
      }
    }

    # ARD-only analyses inherit the output's unanimous code-side provenance
    unmatched_a <- a_idx[!(a_sig %in% c_sig)]
    if (length(unmatched_a) > 0) {
      for (col in c("population", "dataset")) {
        v <- unique(code_an[[col]][c_idx])
        v <- v[!is.na(v) & nzchar(v) & v != "UNKNOWN"]
        if (length(v) == 1) merged[[col]][unmatched_a] <- v
      }
      note(paste0(
        "ASSUMED: ", length(unmatched_a), " analysis/analyses of '", oid,
        "' (", paste(merged$name[unmatched_a], collapse = "; "),
        ") were found in the ARD but not in the programme code; they ",
        "inherited the output's population / dataset - review them."))
    }
    if (any(!c_used)) {
      note(paste0(
        "REVIEW: ", sum(!c_used), " analysis/analyses recovered from the ",
        "code (", paste(code_an$name[c_idx[!c_used]], collapse = "; "),
        ") have no counterpart in the ARD of '", oid,
        "' - they may be conditional, superseded, or mis-parsed; they were ",
        "NOT included."))
    }
  }
  merged$analysis_id <- sprintf("An_%02d", seq_len(nrow(merged)))

  # outputs: the code's pre-defined groups / population win over observation
  outputs <- from_ard$outputs
  for (col in c("population", "groups")) {
    better <- from_code$outputs[[col]]
    keep <- !is.na(better) & nzchar(better)
    outputs[[col]][keep] <- better[keep]
  }
  # per-analysis groups from the code also serve the outputs sheet
  for (i in seq_len(nrow(outputs))) {
    oid <- outputs$output_id[i]
    g <- unique(merged$groups[merged$output_id == oid])
    g <- g[!is.na(g) & nzchar(g)]
    if (length(g) == 1) outputs$groups[i] <- g
    p <- unique(merged$population[merged$output_id == oid])
    p <- p[!is.na(p) & nzchar(p)]
    # a unanimous analysis-level population (e.g. the code's SAFFL) beats a
    # blank output default and the ARD route's assumed "ALL"
    if (length(p) == 1 && (!nzchar(outputs$population[i]) ||
                           toupper(trimws(outputs$population[i])) == "ALL")) {
      outputs$population[i] <- p
    }
  }

  # the ARD side flags what it alone cannot know; drop the alarm when the
  # code side filled it in for every analysis
  if (!any(is.na(merged$dataset) | merged$dataset == "UNKNOWN")) {
    notes <- notes[!grepl("`dataset` is 'UNKNOWN'", notes, fixed = TRUE)]
  }

  list(outputs = outputs, analyses = merged,
       displays = from_code$displays, notes = unique(notes))
}
