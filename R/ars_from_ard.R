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
#'   \item the subject-count idioms become \code{total_n}: cards'
#'     \code{..total_n..} rows, an ungrouped tabulation of a variable that
#'     other analyses use as their grouping (e.g. \code{.by_stats = TRUE}
#'     output), and the ydisctools \code{.flag_} pattern maps to a flat
#'     per-group count.
#' }
#'
#' An ARD carries no data provenance, so \code{dataset} (unless supplied),
#' \code{population} and \code{where} conditions cannot be recovered -- they
#' are surfaced as REVIEW notes, and the observed levels of the first
#' grouping are emitted as the pre-defined \code{groups} with an ASSUMED
#' note.  Like [ars_params_from_code()], the result is a DRAFT to review.
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
  note(paste0("REVIEW: an ARD carries no population or subset conditions - ",
              "`population` and `where` are blank; set them from the ",
              "source programme or the SAP (use population = 'ALL' when ",
              "the analysed data were already the intended analysis set)."))

  outputs <- data.frame(
    output_id  = names(ard_of),
    name       = output_name,
    population = "",
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
.apfa_method_keys <- c("total_n", "categorical_summary", "continuous_summary",
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

# grouping variables of one unit, in group1..groupN order
.apfa_grp_vars <- function(d) {
  out <- character(0)
  for (g in intersect(paste0("group", 1:9), names(d))) {
    v <- unique(d[[g]][!is.na(d[[g]])])
    if (length(v) > 0) out <- c(out, v[1])
  }
  out
}

.apfa_extract_output <- function(a, output_id, note) {
  units <- .apfa_units(a)

  rows <- list()
  emit <- function(name, method, variable, group_by, denominator = "") {
    rows[[length(rows) + 1L]] <<- data.frame(
      output_id = output_id, analysis_id = "",
      name = name, method = method, dataset = "",
      variable = variable, population = "",
      group_by = paste(group_by, collapse = ", "),
      groups = "", groups2 = "", where = "",
      denominator = denominator, stringsAsFactors = FALSE
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
      }
    )
  })
  all_grp_vars <- unique(unlist(lapply(facts, `[[`, "grp_vars")))
  common_grp <- if (length(all_grp_vars) > 0) all_grp_vars[1] else ""

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
      emit(paste0(var, ", n (%)"), "categorical_summary", "USUBJID",
           c(ft$grp_vars, var), denominator = "auto")
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
