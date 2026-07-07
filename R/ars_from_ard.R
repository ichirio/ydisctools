# ============================================================================
#  ars_from_ard.R -- recover compact ARS parameters from an existing ARD
#  (issue #32: when the programme cannot be statically analysed, the ARD
#  itself is the next best evidence of the analyses that were run)
# ============================================================================

#' Recover compact ARS parameters from an ARD
#'
#' Drafts the compact parameter set for [build_ars()] from an existing
#' **Analysis Results Dataset** -- a \pkg{cards} ARD (`ard_stack()` output,
#' possibly `unlist_ard_columns()`-flattened), a siera-generated combined ARD
#' (as returned by [ars_generate_ard()]), or a flattened csv/xlsx export of
#' either.  Use this when the programme that produced the ARD is not
#' available or cannot be read by [ars_params_from_code()].
#'
#' Rows are grouped into analyses -- by `AnalysisId` when the ARD is
#' siera-stamped, otherwise by the grouping-columns/variable combination --
#' and each analysis is classified onto the bundled method catalog:
#'
#' \itemize{
#'   \item continuous statistics (`mean`, `sd`, `median`, `p25`, ...) become
#'     \code{continuous_summary} analyses of the ARD `variable`;
#'   \item `n` / `p` tabulations become \code{categorical_summary} analyses
#'     in the ARS idiom (\code{variable = "USUBJID"}, the category variable
#'     as the \emph{second} grouping, \code{denominator = "auto"});
#'   \item proportion-CI stat sets (\code{conf.low} / \code{conf.high},
#'     cardx context \code{proportion_ci}) become \code{proportion_ci}
#'     analyses, with the confidence level (and CI method, when the ARD
#'     kept those rows) recovered onto \code{options};
#'   \item the subject-count idioms become \code{total_n}: cards'
#'     \code{..total_n..} rows, an ungrouped tabulation of a variable that
#'     other analyses use as their grouping (e.g. \code{.by_stats = TRUE}
#'     output), and the ydisctools \code{.flag_} pattern maps to a flat
#'     per-group count;
#'   \item \code{ard_stack_hierarchical()} ARDs are flattened the same way
#'     [ars_params_from_code()] flattens the call: the
#'     \code{over_variables} rows become \emph{Subjects with at least one
#'     event, n (\%)}, deeper levels drop their hierarchy parents from the
#'     groupings (LIMITATION note), and cards sentinel markers
#'     (\code{..ard_hierarchical_overall..} etc.) never leak into
#'     \code{variable} / \code{group_by} (issue #46) -- an unrecognised
#'     sentinel is skipped with a REVIEW note.
#' }
#'
#' An ARD carries no data provenance, so \code{dataset} (unless supplied)
#' and \code{where} conditions cannot be recovered and are surfaced as
#' REVIEW notes.  \code{population} is set to \code{"ALL"} on the output
#' row: the ARD's numbers were computed from the analysed data as-is, so
#' "no further analysis-set filter" is the faithful recovery -- an ASSUMED
#' note reminds you to change it to a flag condition (e.g. \code{SAFFL})
#' if the built ARS will run on unfiltered source data.  The observed
#' levels of the first grouping are emitted as the pre-defined
#' \code{groups} with an ASSUMED note.  Like [ars_params_from_code()],
#' the result is a DRAFT to review.
#'
#' @param ard An ARD data frame, or the path of a flattened ARD file
#'   (\code{.csv} or \code{.xlsx}).  Must carry at least the
#'   \code{variable} and \code{stat_name} columns; \code{group1} /
#'   \code{group1_level} (etc.), \code{variable_level}, \code{context},
#'   \code{OutputId} / \code{AnalysisId} / \code{MethodId} are used when
#'   present.
#' @param output_id Optional character vector of output IDs.  Default: the
#'   ARD's \code{OutputId} values (one output per ID), or \code{"Out_ard"}
#'   when the column is absent.
#' @param output_name Optional character vector of output display names
#'   (same length as the outputs); default uses the output IDs.
#' @param dataset Dataset name to record on every analysis (an ARD does not
#'   carry it). Default \code{"UNKNOWN"}.
#'
#' @return A named list with elements \code{outputs}, \code{analyses},
#'   \code{displays} (\code{NULL}; an ARD carries no display furniture) and
#'   \code{notes} -- the same shape as [ars_params_from_code()], so
#'   [write_ars_params()] and [build_ars()] accept it unchanged.
#'
#' @seealso [ars_params_from_code()] for recovery from the programme source,
#'   [ars_params_recover()] to merge both sources, [write_ars_params()],
#'   [build_ars()]
#'
#' @examples
#' # a minimal flattened ARD: per-arm N, Age summary, Sex n (%)
#' ard <- data.frame(
#'   group1         = c(rep(NA, 2), rep("TRT01A", 8)),
#'   group1_level   = c(rep(NA, 2), rep(c("Placebo", "Active"), 4)),
#'   variable       = c(rep("TRT01A", 2), rep("AGE", 4), rep("SEX", 4)),
#'   variable_level = c("Placebo", "Active", rep(NA, 4), "F", "F", "M", "M"),
#'   stat_name      = c("n", "n", "mean", "mean", "sd", "sd",
#'                      "n", "n", "p", "p"),
#'   stat           = c(10, 10, 70.1, 71.2, 8.1, 7.9, 6, 5, 0.6, 0.5)
#' )
#' rec <- ars_params_from_ard(ard, output_id = "Out_dm", dataset = "ADSL")
#' rec$analyses[, c("analysis_id", "name", "method", "variable", "group_by")]
#' rec$notes
#'
#' @export
ars_params_from_ard <- function(ard, output_id = NULL, output_name = NULL,
                                dataset = "UNKNOWN") {
  if (is.character(ard) && length(ard) == 1) {
    if (!file.exists(ard)) {
      stop("ARD file not found: '", ard, "'.", call. = FALSE)
    }
    ext <- tolower(sub(".*[.]", "", ard))
    ard <- switch(ext,
      csv  = utils::read.csv(ard, stringsAsFactors = FALSE),
      xlsx = as.data.frame(readxl::read_excel(ard)),
      stop("Unsupported ARD file type '.", ext, "' (use .csv or .xlsx).",
           call. = FALSE)
    )
  }
  if (!is.data.frame(ard)) {
    stop("`ard` must be an ARD data frame or a .csv/.xlsx file path.",
         call. = FALSE)
  }
  ard <- as.data.frame(ard)
  missing_cols <- setdiff(c("variable", "stat_name"), names(ard))
  if (length(missing_cols) > 0) {
    stop("`ard` is missing required column(s): ",
         paste(missing_cols, collapse = ", "), ".", call. = FALSE)
  }

  # normalise the metadata columns: flatten list-columns (raw cards ARDs),
  # coerce to character, blank -> NA
  meta_cols <- intersect(
    c("OutputId", "AnalysisId", "MethodId", "context",
      "variable", "variable_level",
      paste0("group", 1:9), paste0("group", 1:9, "_level")),
    names(ard)
  )
  for (col in meta_cols) {
    v <- ard[[col]]
    if (is.list(v)) {
      v <- vapply(v, function(x) {
        if (is.null(x) || length(x) == 0) NA_character_ else
          as.character(x[[1]])
      }, character(1))
    }
    v <- as.character(v)
    v[!is.na(v) & !nzchar(trimws(v))] <- NA_character_
    ard[[col]] <- v
  }

  notes <- character(0)
  note <- function(msg) notes <<- c(notes, msg)

  # -- outputs ------------------------------------------------------------------
  if ("OutputId" %in% names(ard)) {
    ard_of <- split(ard, factor(ard$OutputId, levels = unique(ard$OutputId)))
  } else {
    ard_of <- list(ard)
    names(ard_of) <- if (is.null(output_id)) "Out_ard" else output_id[1]
  }
  if (!is.null(output_id)) {
    if (length(output_id) != length(ard_of)) {
      stop("`output_id` must have one entry per output (found ",
           length(ard_of), " output(s) in the ARD).", call. = FALSE)
    }
    names(ard_of) <- output_id
  }
  if (is.null(output_name)) output_name <- names(ard_of)
  if (length(output_name) != length(ard_of)) {
    stop("`output_name` must have one entry per output (found ",
         length(ard_of), " output(s) in the ARD).", call. = FALSE)
  }

  parts <- lapply(seq_along(ard_of), function(i) {
    .apfa_extract_output(ard_of[[i]], names(ard_of)[i], note)
  })

  analyses <- do.call(rbind, lapply(parts, `[[`, "analyses"))
  if (is.null(analyses) || nrow(analyses) == 0) {
    stop("No analyses could be recovered from the ARD (no recognisable ",
         "statistic rows).", call. = FALSE)
  }
  analyses$analysis_id <- sprintf("An_%02d", seq_len(nrow(analyses)))
  analyses$dataset <- dataset
  if (identical(dataset, "UNKNOWN")) {
    note(paste0("REVIEW: an ARD carries no dataset names - `dataset` is ",
                "'UNKNOWN' on every analysis; set the `dataset` argument ",
                "or edit the column."))
  }
  note(paste0("ASSUMED: an ARD carries no analysis-set provenance - ",
              "`population` was set to 'ALL' (the ARD was computed from ",
              "the analysed data as-is); change it to a flag condition ",
              "(e.g. SAFFL) if the ARS will run on unfiltered source data. ",
              "`where` subsets cannot be recovered either - set them from ",
              "the source programme or the SAP."))

  outputs <- data.frame(
    output_id  = names(ard_of),
    name       = output_name,
    population = "ALL",
    group_by   = vapply(parts, `[[`, character(1), "group_by"),
    groups     = vapply(parts, `[[`, character(1), "groups"),
    stringsAsFactors = FALSE
  )
  if (any(nzchar(outputs$groups))) {
    note(paste0("ASSUMED: the observed levels of the first grouping were ",
                "emitted as the pre-defined `groups` - confirm they are ",
                "the planned (not just the observed) groups."))
  }

  list(outputs = outputs, analyses = analyses, displays = NULL,
       notes = unique(notes))
}

# ============================================================================
#  Internal machinery (prefix .apfa_ = "ARS params from ARD")
# ============================================================================

.apfa_cont_stats <- c("mean", "sd", "median", "p25", "p75", "min", "max",
                      "var", "iqr", "geo_mean", "cv")

# known catalog keys a siera-stamped MethodId can map back onto
.apfa_method_keys <- c("total_n", "categorical_summary",
                       "categorical_summary_flat", "continuous_summary",
                       "proportion_ci",
                       "risk_difference", "risk_difference_per_group",
                       "fishers_exact", "chisq", "anova")

# split one output's rows into analysis units (AnalysisId when present,
# otherwise the grouping-columns + variable combination), preserving the
# order of first appearance
.apfa_units <- function(a) {
  grp_cols <- intersect(paste0("group", 1:9), names(a))
  key <- if ("AnalysisId" %in% names(a) && !all(is.na(a$AnalysisId))) {
    a$AnalysisId
  } else {
    sig <- a$variable
    for (g in grp_cols) {
      sig <- paste(sig, ifelse(is.na(a[[g]]), "", a[[g]]), sep = "\r")
    }
    if ("context" %in% names(a)) {
      sig <- paste(sig, ifelse(is.na(a$context), "", a$context), sep = "\r")
    }
    sig
  }
  split(a, factor(key, levels = unique(key)))
}

# map a cardx CI method label ("Clopper-Pearson Confidence Interval") back
# to the ard_categorical_ci() method token; NA when unrecognised
.apfa_ci_method_slug <- function(label) {
  l <- tolower(label)
  if (grepl("clopper", l)) return("clopper-pearson")
  if (grepl("wilson", l)) return(if (grepl("continuity", l))
    "wilson.correct" else "wilson")
  if (grepl("wald", l)) return(if (grepl("continuity", l))
    "wald.correct" else "wald")
  if (grepl("agresti", l)) return("agresti-coull")
  if (grepl("jeffreys", l)) return("jeffreys")
  NA_character_
}

# cards internal sentinel variables ("..total_n..", "..ard_total_n..",
# "..ard_hierarchical_overall..", ...) - never real dataset variables
.apfa_is_sentinel <- function(v) grepl("^\\.\\..+\\.\\.$", v)

# grouping variables of one unit, in group1..groupN order (cards sentinel
# values are dropped - they are markers, not variables)
.apfa_grp_vars <- function(d) {
  out <- character(0)
  for (g in intersect(paste0("group", 1:9), names(d))) {
    v <- unique(d[[g]][!is.na(d[[g]])])
    if (length(v) > 0 && !.apfa_is_sentinel(v[1])) out <- c(out, v[1])
  }
  out
}

.apfa_extract_output <- function(a, output_id, note) {
  units <- .apfa_units(a)

  rows <- list()
  emit <- function(name, method, variable, group_by, denominator = "",
                   options = "") {
    rows[[length(rows) + 1L]] <<- data.frame(
      output_id = output_id, analysis_id = "",
      name = name, method = method, dataset = "",
      variable = variable, population = "",
      group_by = paste(group_by, collapse = ", "),
      groups = "", groups2 = "", where = "",
      denominator = denominator, options = options,
      stringsAsFactors = FALSE
    )
  }

  # first pass: collect each unit's facts (a later pass needs to know every
  # grouping variable of the output to spot the big-N idiom)
  facts <- lapply(units, function(d) {
    list(
      var      = unique(d$variable[!is.na(d$variable)])[1],
      grp_vars = .apfa_grp_vars(d),
      stats    = unique(d$stat_name),
      ctx      = if ("context" %in% names(d)) {
        unique(d$context[!is.na(d$context)])
      } else {
        character(0)
      },
      method_id = if ("MethodId" %in% names(d)) {
        unique(d$MethodId[!is.na(d$MethodId)])[1]
      } else {
        NA_character_
      },
      # proportion-CI metadata rows, when the ARD kept them
      conf_level = if ("stat" %in% names(d)) {
        v <- d$stat[d$stat_name == "conf.level"]
        v <- suppressWarnings(as.numeric(if (is.list(v)) unlist(v) else v))
        v <- v[!is.na(v)]
        if (length(v) > 0) v[1] else NA_real_
      } else {
        NA_real_
      },
      ci_method = if ("stat" %in% names(d)) {
        v <- d$stat[d$stat_name == "method"]
        v <- as.character(if (is.list(v)) unlist(v) else v)
        v <- v[!is.na(v)]
        if (length(v) > 0) v[1] else NA_character_
      } else {
        NA_character_
      }
    )
  })
  all_grp_vars <- unique(unlist(lapply(facts, `[[`, "grp_vars")))
  common_grp <- if (length(all_grp_vars) > 0) all_grp_vars[1] else ""

  # hierarchy parents (ard_stack_hierarchical): a variable analysed by one
  # GROUPED unit that also appears as a grouping of another unit (AEBODSYS:
  # its own by-SOC unit + group3 of the by-PT unit). The ungrouped big-N
  # idiom (a tabulation of the by-variable) has no groupings, so it never
  # qualifies.
  parent_vars <- unique(unlist(lapply(facts, function(f) {
    if (!is.na(f$var) && !.apfa_is_sentinel(f$var) &&
        length(f$grp_vars) > 0) f$var else NULL
  })))
  hier_noted <- FALSE

  for (ft in facts) {
    var <- ft$var
    if (is.na(var)) next

    # a siera-stamped MethodId wins when it maps onto a catalog key
    key <- if (!is.na(ft$method_id)) sub("^Mth_", "", ft$method_id) else ""
    known <- key %in% .apfa_method_keys

    is_cont <- any(ft$stats %in% .apfa_cont_stats) ||
      any(ft$ctx %in% c("summary", "continuous"))
    if (known) is_cont <- key == "continuous_summary"

    is_cards_totn <- var %in% c("..total_n..", "..ard_total_n..") ||
      any(ft$ctx == "total_n")
    if (is_cards_totn || (known && key == "total_n")) {
      grp <- if (length(ft$grp_vars) > 0) {
        ft$grp_vars
      } else if (is_cards_totn) {
        note(paste0("ASSUMED: a cards `", var, "` row (overall N) was ",
                    "emitted as a per-group total_n analysis on '",
                    common_grp, "'."))
        common_grp
      } else {
        var
      }
      emit("Number of subjects", "total_n", "USUBJID", grp)
      next
    }
    if (identical(var, ".flag_")) {
      # ydisctools flat pattern: subject count per group
      emit("Subjects, n (%)", "categorical_summary", "USUBJID",
           ft$grp_vars, denominator = "auto")
      next
    }
    if (.apfa_is_sentinel(var)) {
      # cards internal markers are never dataset variables - they must not
      # leak into `variable` / `group_by` (issue #46)
      if (identical(var, "..ard_hierarchical_overall..")) {
        # ard_stack_hierarchical(over_variables = TRUE): the any-event row,
        # same mapping as ars_params_from_code() uses for over_variables
        emit("Subjects with at least one event, n (%)",
             "categorical_summary", "USUBJID", ft$grp_vars,
             denominator = "auto")
        note(paste0("ASSUMED: the hierarchical overall rows ",
                    "(`..ard_hierarchical_overall..`) were emitted as ",
                    "'Subjects with at least one event, n (%)'."))
      } else {
        note(paste0("REVIEW: cards internal rows ('", var, "') were not ",
                    "mapped to a catalog method and were skipped."))
      }
      next
    }
    # proportion with CI (cardx::ard_categorical_ci / the ydisctools
    # proportion_ci method, issue #40)
    if (any(ft$stats %in% c("conf.low", "conf.high")) ||
        any(ft$ctx == "proportion_ci") ||
        (known && startsWith(key, "proportion_ci"))) {
      opts <- character(0)
      if (!is.na(ft$ci_method)) {
        slug <- .apfa_ci_method_slug(ft$ci_method)
        if (!is.na(slug)) opts <- c(opts, paste0("method=", slug))
      }
      if (!is.na(ft$conf_level)) {
        opts <- c(opts, paste0("conf.level=",
                               format(ft$conf_level, trim = TRUE)))
      }
      emit(paste0(var, " proportion CI"), "proportion_ci", var,
           ft$grp_vars, options = paste(opts, collapse = "; "))
      note(paste0("REVIEW: a proportion-CI analysis of '", var, "' was ",
                  "recovered; check the `options` column (CI method / ",
                  "conf.level / a value= level restriction cannot always ",
                  "be read back from an ARD)."))
      next
    }
    if (is_cont) {
      emit(paste0(var, " (continuous)"), "continuous_summary", var,
           ft$grp_vars)
      next
    }
    if (any(ft$stats %in% c("n", "N", "p"))) {
      if (length(ft$grp_vars) == 0 && var %in% all_grp_vars) {
        # ungrouped tabulation of the grouping variable = the big N
        # (cards .by_stats = TRUE, or a siera big-N analysis)
        emit("Number of subjects", "total_n", "USUBJID", var)
        next
      }
      # a nested display's deeper level (the by-PT unit) carries its
      # hierarchy parent (AEBODSYS) as a grouping; flatten it away, the
      # same way ars_params_from_code() flattens ard_stack_hierarchical()
      grp <- ft$grp_vars
      hier <- intersect(grp, parent_vars)
      hier <- setdiff(hier, var)
      if (length(hier) > 0) {
        grp <- setdiff(grp, hier)
        if (!hier_noted) {
          hier_noted <- TRUE
          note(paste0("LIMITATION: the ARD is hierarchical (nested ",
                      "groupings incl. ", paste(hier, collapse = ", "),
                      "); the compact format supports 2 simultaneous ",
                      "groupings, so FLAT per-level analyses were ",
                      "generated instead (ydisctools #6)."))
        }
      }
      emit(paste0(var, ", n (%)"), "categorical_summary", "USUBJID",
           c(grp, var), denominator = "auto")
      next
    }
    note(paste0("REVIEW: analysis of '", var, "' with statistic(s) ",
                paste(ft$stats, collapse = "/"), " was not mapped to a ",
                "catalog method - add it to the Analyses sheet manually."))
  }

  if (length(rows) > 0) {
    note(paste0("ASSUMED: n (%) tabulations were mapped to the ARS idiom ",
                "(variable = USUBJID, the category variable as the second ",
                "grouping, denominator = 'auto')."))
    # cards .total_n = TRUE yields BOTH an ungrouped tabulation of the
    # grouping variable and an `..ard_total_n..` row - keep one total_n
    # per signature
    an_df <- do.call(rbind, rows)
    is_totn <- an_df$method == "total_n"
    dup <- is_totn & duplicated(paste(an_df$method, an_df$variable,
                                      an_df$group_by, sep = "\r"))
    rows <- rows[!dup]
    # denominators first: cards lists .total_n / .by_stats counts LAST, but
    # siera generates programmes in row order and the percentage analyses
    # read the denominator's data frame - it must already exist (issue #37)
    is_totn <- is_totn[!dup]
    rows <- c(rows[is_totn], rows[!is_totn])

    # the compact format / build_ars() cap at 2 simultaneous groupings -
    # keep the faithful group list (as ars_params_from_code() does for a
    # multi-`by` tabulation, #40) but say so up front
    n_grp <- vapply(rows, function(r) {
      length(strsplit(r$group_by, ",", fixed = TRUE)[[1]])
    }, integer(1))
    if (any(n_grp > 2)) {
      over <- vapply(rows[n_grp > 2], `[[`, character(1), "name")
      note(paste0("LIMITATION: ", sum(n_grp > 2), " analysis/analyses use ",
                  "3+ simultaneous groupings (",
                  paste(over, collapse = "; "), "); the compact format ",
                  "supports at most 2, so build_ars() will reject them - ",
                  "split the display or drop a grouping (ydisctools #6)."))
    }
  }

  # observed levels of the common first grouping -> pre-defined groups
  groups <- ""
  if (nzchar(common_grp)) {
    for (g in intersect(paste0("group", 1:9), names(a))) {
      lv_col <- paste0(g, "_level")
      if (!lv_col %in% names(a)) next
      hit <- !is.na(a[[g]]) & a[[g]] == common_grp
      lv <- unique(a[[lv_col]][hit])
      lv <- lv[!is.na(lv)]
      if (length(lv) > 0) {
        groups <- paste(lv, collapse = " | ")
        break
      }
    }
  }

  list(
    analyses = if (length(rows) > 0) do.call(rbind, rows) else NULL,
    group_by = common_grp,
    groups = groups
  )
}
