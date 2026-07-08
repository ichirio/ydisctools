# ============================================================================
#  ars_tools.R -- Generate siera-compatible CDISC ARS workbooks from
#  compact parameters
# ============================================================================
#
#  The pharmaverse package {siera} (siera::readARS()) turns CDISC Analysis
#  Results Standard (ARS) metadata into runnable {cards}-based ARD programmes,
#  but the ARS workbook it consumes is verbose: nine interlinked sheets with
#  heavily repeated information.  The functions in this file expand a compact
#  parameter set (one row per analysis) into that workbook:
#
#    ars_param_template()  write a compact parameter workbook (with a worked
#                          demographics + AE example) to copy per study
#    read_ars_params()     read a compact parameter workbook back in
#    build_ars()           expand compact parameters into ARS sheet tables
#    write_ars_xlsx()      write the tables to a siera-readable .xlsx
#
#  Method metadata (AnalysisMethods / AnalysisMethodCodeTemplate /
#  AnalysisMethodCodeParameters) is expanded from a vendored copy of siera's
#  MIT-licensed method template catalog (inst/ars-method-library/), so the
#  compact format never asks the user for template code.
#
#  JSON output is deliberately not implemented here: siera (> 0.5.6; in the
#  development version at the time of writing) ships ars_xlsx_to_json(), a
#  faithful port of CDISC's excel2ars.py, which converts the generated
#  workbook when a JSON ARS is required.
# ============================================================================

# -- Method catalog -----------------------------------------------------------

# Read the method-template catalog: the vendored siera catalog
# (method-library.json, kept verbatim) merged with the ydisctools overlay
# (method-library-ydisctools.json) -- an overlay entry with the same id
# REPLACES the vendored one.  Returns a named list of method entries
# (name/label/description/operations/parameters/templateCode), with the
# catalog-level template context stored in attr(, "context").
.ars_method_catalog <- function() {
  path <- system.file("ars-method-library", "method-library.json",
                      package = "ydisctools", mustWork = TRUE)
  cat_json <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  methods <- cat_json$methods
  names(methods) <- vapply(methods, function(m) m$id, character(1))

  overlay_path <- system.file("ars-method-library",
                              "method-library-ydisctools.json",
                              package = "ydisctools")
  if (nzchar(overlay_path)) {
    overlay <- jsonlite::fromJSON(overlay_path, simplifyVector = FALSE)
    for (m in overlay$methods) {
      methods[[m$id]] <- m
    }
  }
  attr(methods, "context") <-
    if (!is.null(cat_json$context)) cat_json$context else "R (siera)"
  methods
}

# -- Condition mini-syntax ----------------------------------------------------

.ars_comparators <- c("EQ", "NE", "GT", "GE", "LT", "LE",
                      "IN", "NOTIN", "CONTAINS")

# Parse ONE condition token into a list(dataset, variable, comparator, value).
#
#   "SAFFL"                  -> default_dataset.SAFFL EQ "Y"   (bare-flag sugar)
#   "TRTEMFL EQ Y"           -> default_dataset.TRTEMFL EQ "Y"
#   "ADSL.AGE GE 65"         -> ADSL.AGE GE "65"
#   "SEX IN M|F"             -> IN with pipe-separated values (kept as-is;
#                               siera's xlsx reader splits on the pipe)
.ars_parse_condition <- function(token, default_dataset) {
  token <- trimws(token)
  m0 <- regmatches(
    token,
    regexec("^(?:([A-Za-z][A-Za-z0-9_]*)\\.)?([A-Za-z][A-Za-z0-9_]*)$", token)
  )[[1]]
  if (length(m0) == 3) {   # bare flag, optionally dataset-qualified
    return(list(dataset = if (nzchar(m0[2])) m0[2] else default_dataset,
                variable = m0[3], comparator = "EQ", value = "Y"))
  }
  rx <- paste0(
    "^(?:([A-Za-z][A-Za-z0-9_]*)\\.)?",   # optional dataset prefix
    "([A-Za-z][A-Za-z0-9_]*)\\s+",        # variable
    "(", paste(.ars_comparators, collapse = "|"), ")\\s+",
    "(.+?)\\s*$"                          # value (rest of the token)
  )
  m <- regmatches(token, regexec(rx, token, perl = TRUE))[[1]]
  if (length(m) == 0) {
    stop("Cannot parse ARS condition: '", token, "'. Expected a bare flag ",
         "name (e.g. 'SAFFL') or '[DATASET.]VARIABLE COMPARATOR VALUE' with ",
         "comparator one of ", paste(.ars_comparators, collapse = ", "), ".",
         call. = FALSE)
  }
  dataset <- if (nzchar(m[2])) m[2] else default_dataset
  value <- m[5]
  if (m[4] %in% c("IN", "NOTIN")) {
    value <- gsub("\\s*\\|\\s*", "|", value)
  }
  list(dataset = dataset, variable = m[3], comparator = m[4], value = value)
}

# Parse a ";"-separated AND chain of condition tokens into a data.frame with
# one row per condition (columns dataset/variable/comparator/value).
.ars_parse_condition_chain <- function(x, default_dataset) {
  tokens <- trimws(strsplit(x, ";", fixed = TRUE)[[1]])
  tokens <- tokens[nzchar(tokens)]
  if (length(tokens) == 0) {
    stop("Empty ARS condition: '", x, "'.", call. = FALSE)
  }
  rows <- lapply(tokens, .ars_parse_condition, default_dataset = default_dataset)
  data.frame(
    dataset    = vapply(rows, `[[`, character(1), "dataset"),
    variable   = vapply(rows, `[[`, character(1), "variable"),
    comparator = vapply(rows, `[[`, character(1), "comparator"),
    value      = vapply(rows, `[[`, character(1), "value"),
    stringsAsFactors = FALSE
  )
}

# Fold every ARS dataset reference (Analyses$dataset,
# AnalysisSets$condition_dataset, DataSubsets$condition_dataset) to a single
# spelling per case-insensitive key. siera reads one file per distinct
# spelling, so mixed casing for one logical dataset would read it twice; the
# first spelling seen -- Analyses$dataset comes first, i.e. the user's own --
# wins. Datasets that only differ by name (AE's ADAE vs an ADSL population)
# fold to different keys and stay separate, as they should.
.ars_canonicalize_datasets <- function(Analyses, AnalysisSets, DataSubsets) {
  spellings <- c(Analyses$dataset,
                 AnalysisSets$condition_dataset,
                 DataSubsets$condition_dataset)
  spellings <- spellings[!is.na(spellings) & nzchar(spellings)]
  if (length(spellings) == 0) {
    return(list(Analyses = Analyses, AnalysisSets = AnalysisSets,
                DataSubsets = DataSubsets))
  }
  canon <- spellings[!duplicated(toupper(spellings))]
  names(canon) <- toupper(canon)
  fold <- function(x) {
    if (is.null(x)) return(x)
    hit <- !is.na(x) & nzchar(x)
    x[hit] <- canon[toupper(x[hit])]
    x
  }
  Analyses$dataset <- fold(Analyses$dataset)
  AnalysisSets$condition_dataset <- fold(AnalysisSets$condition_dataset)
  DataSubsets$condition_dataset <- fold(DataSubsets$condition_dataset)
  list(Analyses = Analyses, AnalysisSets = AnalysisSets,
       DataSubsets = DataSubsets)
}

# Friendly population names for common ADaM flags.
.ars_population_name <- function(expr, cond) {
  if (identical(toupper(trimws(as.character(expr))), "ALL")) {
    return("All Subjects")
  }
  flags <- c(SAFFL = "Safety Population",
             ITTFL = "Intent-to-Treat Population",
             FASFL = "Full Analysis Set",
             PPROTFL = "Per-Protocol Population",
             RANDFL = "Randomized Population",
             ENRLFL = "Enrolled Population",
             COMPLFL = "Completers Population")
  if (nrow(cond) == 1 && cond$comparator == "EQ" && cond$value == "Y" &&
      cond$variable %in% names(flags)) {
    unname(flags[cond$variable])
  } else {
    paste0("Population: ", expr)
  }
}

# Parse a group_by spec ("TRT01A" / "ADSL.TRT01A" / comma-separated for
# multiple groupings) into a data.frame(dataset, variable).
.ars_parse_group_by <- function(x, default_dataset = "ADSL") {
  vars <- trimws(strsplit(x, ",", fixed = TRUE)[[1]])
  vars <- vars[nzchar(vars)]
  if (length(vars) == 0) {
    stop("Empty group_by specification: '", x, "'.", call. = FALSE)
  }
  parse_one <- function(v) {
    m <- regmatches(
      v,
      regexec("^(?:([A-Za-z][A-Za-z0-9_]*)\\.)?([A-Za-z][A-Za-z0-9_]*)$",
              v, perl = TRUE)
    )[[1]]
    if (length(m) == 0) {
      stop("Cannot parse group_by entry: '", v,
           "'. Expected 'VARIABLE' or 'DATASET.VARIABLE'.", call. = FALSE)
    }
    c(dataset = if (nzchar(m[2])) m[2] else default_dataset, variable = m[3])
  }
  parsed <- t(vapply(vars, parse_one, character(2)))
  data.frame(dataset = parsed[, "dataset"], variable = parsed[, "variable"],
             stringsAsFactors = FALSE, row.names = NULL)
}

# Parse an analysis `options` cell ("method=wilson; conf.level=0.9;
# value=Response") into a named character vector.
.ars_parse_options <- function(x) {
  if (.ars_blank(x)) return(character(0))
  tokens <- trimws(strsplit(x, ";", fixed = TRUE)[[1]])
  tokens <- tokens[nzchar(tokens)]
  m <- regmatches(tokens,
                  regexec("^([A-Za-z][A-Za-z0-9._]*)\\s*=\\s*(.+?)\\s*$",
                          tokens))
  bad <- vapply(m, length, integer(1)) != 3
  if (any(bad)) {
    stop("Cannot parse `options` entry: '", tokens[bad][1],
         "'. Expected 'key=value' pairs separated by ';'.", call. = FALSE)
  }
  stats::setNames(vapply(m, `[[`, character(1), 3),
                  vapply(m, `[[`, character(1), 2))
}

# Split a pipe-separated group-levels spec into a character vector (NULL when
# blank -> data-driven grouping).
.ars_parse_groups <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(trimws(x))) {
    return(NULL)
  }
  levels <- trimws(strsplit(x, "|", fixed = TRUE)[[1]])
  levels <- levels[nzchar(levels)]
  if (length(levels) == 0) NULL else levels
}

# -- Small utilities ----------------------------------------------------------

.ars_chr <- function(x) {
  if (is.null(x) || length(x) == 0) NA_character_ else as.character(x)[1]
}

.ars_blank <- function(x) {
  is.null(x) || length(x) == 0 || is.na(x) || !nzchar(trimws(as.character(x)))
}

# Ensure the data.frame has all `cols`; add missing ones filled with NA.
.ars_ensure_cols <- function(df, cols) {
  for (col in cols) {
    if (!col %in% names(df)) df[[col]] <- NA_character_
  }
  df
}

# -- Parameter template and reader --------------------------------------------

#' Write a compact ARS parameter workbook template
#'
#' Creates the compact parameter workbook consumed by [build_ars()], pre-filled
#' with a worked example (a demographics summary and an adverse-event summary
#' on CDISC-pilot-style ADaM data).  Copy the file per study, replace the
#' example rows, and feed it back through [read_ars_params()] / [build_ars()] /
#' [write_ars_xlsx()] to obtain a full ARS workbook for
#' \code{siera::readARS()}.
#'
#' The workbook has three sheets:
#' \describe{
#'   \item{\code{Study}}{key/value pairs (\code{study_id}, \code{study_title});
#'     used for workbook provenance only.}
#'   \item{\code{Outputs}}{one row per output (display): \code{output_id},
#'     \code{name}, and optional defaults \code{population}, \code{group_by},
#'     \code{groups} inherited by the output's analyses.}
#'   \item{\code{Analyses}}{one row per analysis: \code{output_id},
#'     \code{method} (a key of the bundled method catalog, e.g.
#'     \code{"total_n"}, \code{"categorical_summary"},
#'     \code{"continuous_summary"}, \code{"proportion_ci"}),
#'     \code{dataset}, \code{variable}, and
#'     optional \code{analysis_id}, \code{name}, \code{population},
#'     \code{group_by}, \code{groups}, \code{groups2}, \code{where},
#'     \code{denominator}, \code{options}.}
#'   \item{\code{Displays}}{(optional) display furniture, one row per
#'     subsection: \code{output_id}, \code{section_type} (\code{Header},
#'     \code{Title}, \code{Footnote}, \code{Footer}, \code{Abbreviations},
#'     \code{Rowlabel Header}), \code{order}, \code{text}.  When present,
#'     [build_ars()] emits the ARS \code{Displays} /
#'     \code{GlobalDisplaySections} sheets (siera does not consume them, but
#'     they complete the ARS metadata).}
#' }
#'
#' Conditions (\code{population}, \code{where}) use a mini syntax: a bare flag
#' name (\code{"SAFFL"} means \code{ADSL.SAFFL EQ Y};
#' \code{"ADRS.ITTFL"} keeps its dataset), or
#' \code{"[DATASET.]VARIABLE COMPARATOR VALUE"} with comparators
#' \code{EQ, NE, GT, GE, LT, LE, IN, NOTIN, CONTAINS}; chain several with
#' \code{";"} (combined with AND); \code{IN}/\code{NOTIN} values are
#' pipe-separated (\code{"SEX IN M|F"}).
#' The \code{"proportion_ci"} method (per-group proportion of each level of
#' the analysis variable with its confidence interval, generated with
#' \code{cardx::ard_categorical_ci()}) takes per-analysis \code{options}:
#' \code{"method=clopper-pearson; conf.level=0.9; value=Response"} -- the CI
#' method (default \code{clopper-pearson}), the confidence level (default
#' \code{0.95}) and an optional single response level.  [build_ars()] bakes
#' each distinct option set into its own generated method entry, so several
#' CI flavours can coexist in one study.
#'
#' \code{population} additionally accepts \code{"ALL"} -- no analysis-set
#' filter at all, for data that already \emph{are} the intended analysis set
#' (e.g. pre-filtered upstream).  \code{build_ars()} then emits no
#' \code{AnalysisSets} row and a blank \code{analysisSetId}, and
#' \code{siera::readARS()} uses the analysis dataset unfiltered (its
#' documented fallback; the generation warns "using the analysis dataset
#' without filtering").  An \code{"ALL"} output must keep all its analyses
#' on one dataset.
#'
#'
#' @param path File path of the workbook to write (\code{.xlsx}).
#' @param overwrite Logical; overwrite an existing file? Default \code{FALSE}.
#'
#' @return The path, invisibly.
#'
#' @seealso [read_ars_params()], [build_ars()], [write_ars_xlsx()]
#'
#' @examples
#' tmp <- file.path(tempdir(), "ars_params.xlsx")
#' ars_param_template(tmp, overwrite = TRUE)
#' params <- read_ars_params(tmp)
#' names(params)
#'
#' @export
ars_param_template <- function(path, overwrite = FALSE) {
  if (file.exists(path) && !overwrite) {
    stop("File already exists: '", path, "'. Set overwrite = TRUE to replace.",
         call. = FALSE)
  }
  study <- data.frame(
    key   = c("study_id", "study_title"),
    value = c("STUDY01", "Example Study"),
    stringsAsFactors = FALSE
  )
  outputs <- data.frame(
    output_id  = c("Out_demog", "Out_teae"),
    name       = c("Summary of Demographic Data",
                   "Summary of Treatment-Emergent Adverse Events"),
    population = c("SAFFL", "SAFFL"),
    group_by   = c("TRT01A", "TRT01A"),
    groups     = c("Placebo | Active", ""),
    stringsAsFactors = FALSE
  )
  # Note the ARS idiom for "n (%) of subjects per category": the analysis
  # variable stays USUBJID and the category variable (AGEGR1, SEX, AEBODSYS,
  # ...) enters as the SECOND grouping.  The demographics output mirrors a
  # typical DM display: big N, Age (continuous), two age groupings, Sex,
  # Race and Ethnicity.
  analyses <- data.frame(
    output_id   = c(rep("Out_demog", 7), rep("Out_teae", 3)),
    analysis_id = sprintf("An_%02d", 1:10),
    name        = c("Number of subjects", "Age (years)", "Age group 1",
                    "Age group 2", "Sex", "Race", "Ethnicity",
                    "Number of subjects", "TEAE by System Organ Class",
                    "TEAE by severity"),
    method      = c("total_n", "continuous_summary",
                    rep("categorical_summary", 5),
                    "total_n", "categorical_summary", "categorical_summary"),
    dataset     = c(rep("ADSL", 7), "ADSL", "ADAE", "ADAE"),
    variable    = c("USUBJID", "AGE", rep("USUBJID", 8)),
    population  = "",
    group_by    = c("", "", "TRT01A, AGEGR1", "TRT01A, AGEGR2",
                    "TRT01A, SEX", "TRT01A, RACE", "TRT01A, ETHNIC",
                    "", "TRT01A, ADAE.AEBODSYS", "TRT01A, ADAE.AESEV"),
    groups      = "",
    groups2     = "",
    where       = c(rep("", 8), "TRTEMFL EQ Y", "TRTEMFL EQ Y"),
    denominator = c("", "", rep("auto", 5), "", "An_08", "An_08"),
    stringsAsFactors = FALSE
  )
  displays <- data.frame(
    output_id    = c("Out_demog", "Out_demog", "Out_demog", "Out_demog",
                     "Out_demog"),
    section_type = c("Header", "Title", "Title", "Footnote", "Footer"),
    order        = c(1L, 1L, 2L, 1L, 1L),
    text         = c("Protocol STUDY01",
                     "Table 14.1.1",
                     "Summary of Demographic Data",
                     "Note: Percentages are based on the number of subjects in the safety population.",
                     "Source: ADSL"),
    stringsAsFactors = FALSE
  )
  writexl::write_xlsx(
    list(Study = study, Outputs = outputs, Analyses = analyses,
         Displays = displays),
    path
  )
  invisible(path)
}

#' Read a compact ARS parameter workbook
#'
#' Reads a workbook in the format written by [ars_param_template()] back into
#' a named list of data frames suitable for [build_ars()].
#'
#' @param path Path to the compact parameter workbook (\code{.xlsx}).
#'
#' @return A named list with elements \code{study} (may be \code{NULL}),
#'   \code{outputs}, \code{analyses} and \code{displays} (\code{NULL} when
#'   the workbook has no \code{Displays} sheet).
#'
#' @seealso [ars_param_template()], [build_ars()]
#'
#' @examples
#' tmp <- file.path(tempdir(), "ars_params.xlsx")
#' ars_param_template(tmp, overwrite = TRUE)
#' params <- read_ars_params(tmp)
#' params$outputs
#'
#' @export
read_ars_params <- function(path) {
  if (!file.exists(path)) {
    stop("Parameter workbook not found: '", path, "'.", call. = FALSE)
  }
  sheets <- readxl::excel_sheets(path)
  need <- c("Outputs", "Analyses")
  missing <- setdiff(need, sheets)
  if (length(missing) > 0) {
    stop("Parameter workbook is missing required sheet(s): ",
         paste(missing, collapse = ", "), ".", call. = FALSE)
  }
  as_chr_df <- function(df) {
    out <- as.data.frame(df, stringsAsFactors = FALSE)
    out[] <- lapply(out, function(col) {
      col <- as.character(col)
      col[!is.na(col) & !nzchar(trimws(col))] <- NA_character_
      col
    })
    out
  }
  study <- if ("Study" %in% sheets) {
    as_chr_df(readxl::read_excel(path, sheet = "Study"))
  } else {
    NULL
  }
  displays <- if ("Displays" %in% sheets) {
    as_chr_df(readxl::read_excel(path, sheet = "Displays"))
  } else {
    NULL
  }
  list(
    study    = study,
    outputs  = as_chr_df(readxl::read_excel(path, sheet = "Outputs")),
    analyses = as_chr_df(readxl::read_excel(path, sheet = "Analyses")),
    displays = displays
  )
}

# -- build_ars() --------------------------------------------------------------

#' Build ARS metadata tables from compact parameters
#'
#' Expands a compact parameter set (one row per analysis; see
#' [ars_param_template()] for the format) into the full set of CDISC Analysis
#' Results Standard metadata tables required by \code{siera::readARS()}:
#' contents lists, analysis sets, groupings, data subsets, analyses, and the
#' analysis-method sheets (expanded from the bundled siera method catalog).
#' Repeated structures are de-duplicated and IDs are assigned automatically;
#' numerator/denominator wiring for percentage-based methods is resolved from
#' the \code{denominator} column (\code{"auto"} finds the \code{total_n}
#' analysis with the same population and first grouping in the same output).
#' An ARS \code{Outputs} sheet is always emitted; when the parameters carry
#' display metadata (a \code{Displays} sheet / element, see
#' [ars_param_template()]), the ARS \code{Displays} and
#' \code{GlobalDisplaySections} sheets are emitted as well.
#'
#' @param params Either the path to a compact parameter workbook (read via
#'   [read_ars_params()]) or a named list with elements \code{outputs} and
#'   \code{analyses} (and optionally \code{study}) as returned by
#'   [read_ars_params()].
#'
#' @return A named list of data frames, one per ARS workbook sheet, ready for
#'   [write_ars_xlsx()].
#'
#' @section Sizing note:
#'   \code{siera::readARS()} (as of siera 0.5.x) indexes the third analysis of
#'   each output while deciding how to build the population filter, so
#'   \code{build_ars()} requires at least three analyses per output.
#'
#' @section Worked example set:
#'   \code{system.file("ars-examples", package = "ydisctools")} ships the full
#'   artefact chain for a Demographics and an Adverse Events display: the
#'   compact parameter workbook, the ARS workbook built from it, the ARD
#'   programmes \code{siera::readARS()} generated, and dummy ADaM data the
#'   programmes run on (see the README there).
#'
#' @seealso [ars_param_template()] for the parameter format,
#'   [write_ars_xlsx()] to write the result, and
#'   \code{siera::ars_xlsx_to_json()} (siera > 0.5.6) to convert the written
#'   workbook to the JSON ARS representation.
#'
#' @examples
#' tmp <- file.path(tempdir(), "ars_params.xlsx")
#' ars_param_template(tmp, overwrite = TRUE)
#' ars <- build_ars(tmp)
#' names(ars)
#' ars$Analyses
#'
#' @export
build_ars <- function(params) {
  if (is.character(params) && length(params) == 1) {
    params <- read_ars_params(params)
  }
  if (!is.list(params) || !all(c("outputs", "analyses") %in% names(params))) {
    stop("`params` must be a parameter workbook path or a list with ",
         "`outputs` and `analyses` elements.", call. = FALSE)
  }

  outputs  <- .ars_ensure_cols(as.data.frame(params$outputs,
                                             stringsAsFactors = FALSE),
                               c("output_id", "name", "population",
                                 "group_by", "groups"))
  analyses <- .ars_ensure_cols(as.data.frame(params$analyses,
                                             stringsAsFactors = FALSE),
                               c("output_id", "analysis_id", "name", "method",
                                 "dataset", "variable", "population",
                                 "group_by", "groups", "groups2", "where",
                                 "denominator", "options"))

  # Normalise blank cells ("" / whitespace) to NA so that defaults inherit the
  # same way whether the input came from a workbook or a hand-built data.frame.
  blank_to_na <- function(df) {
    df[] <- lapply(df, function(col) {
      col <- as.character(col)
      col[!is.na(col) & !nzchar(trimws(col))] <- NA_character_
      col
    })
    df
  }
  outputs <- blank_to_na(outputs)
  analyses <- blank_to_na(analyses)

  if (nrow(outputs) == 0) stop("`outputs` has no rows.", call. = FALSE)
  if (nrow(analyses) == 0) stop("`analyses` has no rows.", call. = FALSE)
  if (anyNA(outputs$output_id) || anyNA(outputs$name)) {
    stop("Every `outputs` row needs `output_id` and `name`.", call. = FALSE)
  }
  if (anyDuplicated(outputs$output_id)) {
    stop("Duplicated `output_id` in `outputs`.", call. = FALSE)
  }
  bad_out <- setdiff(analyses$output_id, outputs$output_id)
  if (anyNA(analyses$output_id) || length(bad_out) > 0) {
    stop("`analyses$output_id` values not present in `outputs`: ",
         paste(unique(bad_out), collapse = ", "), ".", call. = FALSE)
  }

  catalog <- .ars_method_catalog()

  # Assign analysis IDs and validate the required per-row fields.
  auto_id <- sprintf("An_%02d", seq_len(nrow(analyses)))
  analyses$analysis_id <- ifelse(is.na(analyses$analysis_id),
                                 auto_id, analyses$analysis_id)
  if (anyDuplicated(analyses$analysis_id)) {
    stop("Duplicated `analysis_id` in `analyses`.", call. = FALSE)
  }
  bad_id <- !grepl("^[A-Za-z0-9_]+$", analyses$analysis_id)
  if (any(bad_id)) {
    stop("`analysis_id` must contain only letters, digits and underscores ",
         "(it becomes part of R object names in the generated programme): ",
         paste(analyses$analysis_id[bad_id], collapse = ", "), ".",
         call. = FALSE)
  }
  for (i in seq_len(nrow(analyses))) {
    for (col in c("method", "dataset", "variable")) {
      if (.ars_blank(analyses[[col]][i])) {
        stop("`analyses` row ", i, " (", analyses$analysis_id[i],
             "): `", col, "` is required.", call. = FALSE)
      }
    }
    if (!analyses$method[i] %in% names(catalog)) {
      stop("`analyses` row ", i, " (", analyses$analysis_id[i],
           "): unknown method '", analyses$method[i], "'. Available: ",
           paste(names(catalog), collapse = ", "), ".", call. = FALSE)
    }
  }

  # siera::readARS() (0.5.x) inspects the 3rd analysis of every output while
  # building the population filter; fewer than 3 analyses per output breaks
  # its code generation, so fail early with a clear message.
  n_per_out <- table(analyses$output_id)
  too_small <- names(n_per_out)[n_per_out < 3]
  if (length(too_small) > 0) {
    stop("siera::readARS() requires at least 3 analyses per output ",
         "(it inspects the output's 3rd analysis when building the ",
         "population filter). Output(s) with fewer: ",
         paste(too_small, collapse = ", "), ".", call. = FALSE)
  }

  # Inherit output-level defaults.
  out_idx <- match(analyses$output_id, outputs$output_id)
  for (col in c("population", "group_by", "groups")) {
    inherit <- is.na(analyses[[col]])
    analyses[[col]][inherit] <- outputs[[col]][out_idx[inherit]]
  }

  for (i in seq_len(nrow(analyses))) {
    miss <- c(if (.ars_blank(analyses$population[i])) "population",
              if (.ars_blank(analyses$group_by[i])) "group_by")
    if (length(miss) > 0) {
      stop("`analyses` row ", i, " (", analyses$analysis_id[i], "): `",
           paste(miss, collapse = "` and `"), "` ",
           if (length(miss) > 1) "are" else "is",
           " required (set on the analysis row or as a default on the ",
           "output row",
           if ("population" %in% miss) {
             paste0("; use population = \"ALL\" when the data are already ",
                    "the intended analysis set and no filter applies")
           },
           ").", call. = FALSE)
    }
  }

  # --- Analysis sets (deduplicated single-condition populations) -----------
  # population = "ALL" means "no analysis-set filter": siera's generated code
  # then uses the analysis dataset unfiltered (its documented fallback for a
  # blank analysisSetId, with a run-time warning).
  is_all <- toupper(trimws(analyses$population)) == "ALL"
  pop_conds <- vector("list", nrow(analyses))
  pop_sig <- character(nrow(analyses))
  for (i in which(!is_all)) {
    cond <- .ars_parse_condition_chain(analyses$population[i],
                                       default_dataset = "ADSL")
    if (nrow(cond) > 1) {
      stop("`population` must be a single condition (siera applies one ",
           "analysis-set condition): '", analyses$population[i], "'.",
           call. = FALSE)
    }
    pop_conds[[i]] <- cond
    pop_sig[i] <- paste(cond$dataset, cond$variable, cond$comparator,
                        cond$value, sep = "\r")
  }
  pop_ids <- rep("", nrow(analyses))
  nz <- which(!is_all)
  if (length(nz) > 0) {
    pop_ids[nz] <- sprintf("AnalysisSet_%02d",
                           match(pop_sig[nz], unique(pop_sig[nz])))
  }
  analyses$.anset_id <- pop_ids

  # One population per output (siera builds df_pop once per output, from the
  # first analysis).
  for (oid in outputs$output_id) {
    sel <- analyses$output_id == oid
    sets <- unique(analyses$.anset_id[sel])
    if (length(sets) > 1) {
      stop("Output '", oid, "' mixes different populations; siera applies ",
           "one analysis set per output.", call. = FALSE)
    }
    # siera's no-filter fallback sets df_pop to the FIRST analysis's dataset,
    # without the cross-dataset USUBJID merge the filtered path performs
    if (any(is_all[sel])) {
      ds <- unique(analyses$dataset[sel])
      if (length(ds) > 1) {
        stop("Output '", oid, "': population = \"ALL\" (no analysis-set ",
             "filter) requires every analysis of the output to use the ",
             "same dataset, but it uses: ", paste(ds, collapse = ", "),
             ". Give the output a population condition, or split it per ",
             "dataset.", call. = FALSE)
      }
    }
  }

  u <- nz[!duplicated(pop_sig[nz])]
  AnalysisSets <- data.frame(
    id = pop_ids[u],
    name = vapply(u, function(k) {
      .ars_population_name(analyses$population[k], pop_conds[[k]])
    }, character(1)),
    description = rep(NA_character_, length(u)),
    level = rep(1L, length(u)), order = rep(1L, length(u)),
    compoundExpression_logicalOperator = rep(NA_character_, length(u)),
    condition_dataset    = vapply(pop_conds[u], `[[`, character(1), "dataset"),
    condition_variable   = vapply(pop_conds[u], `[[`, character(1), "variable"),
    condition_comparator = vapply(pop_conds[u], `[[`, character(1), "comparator"),
    condition_value      = vapply(pop_conds[u], `[[`, character(1), "value"),
    stringsAsFactors = FALSE, row.names = NULL
  )

  # --- Groupings (deduplicated; pre-defined levels or data-driven) ----------
  grp_specs <- list()   # signature -> list(dataset, variable, levels)
  ana_grp_ids <- vector("list", nrow(analyses))
  for (i in seq_len(nrow(analyses))) {
    gb <- .ars_parse_group_by(analyses$group_by[i])
    if (nrow(gb) > 2) {
      stop("`analyses` row ", i, " (", analyses$analysis_id[i],
           "): at most 2 simultaneous groupings are supported.", call. = FALSE)
    }
    level_specs <- list(.ars_parse_groups(analyses$groups[i]),
                        .ars_parse_groups(analyses$groups2[i]))
    ids <- character(nrow(gb))
    for (g in seq_len(nrow(gb))) {
      levels <- level_specs[[g]]
      sig <- paste(gb$dataset[g], gb$variable[g],
                   paste(levels, collapse = "\r"), sep = "\n")
      if (is.null(grp_specs[[sig]])) {
        grp_specs[[sig]] <- list(dataset = gb$dataset[g],
                                 variable = gb$variable[g],
                                 levels = levels)
      }
      ids[g] <- sig
    }
    ana_grp_ids[[i]] <- ids
  }
  grp_ids <- sprintf("AnlsGrp_%02d_%s", seq_along(grp_specs),
                     vapply(grp_specs, `[[`, character(1), "variable"))
  names(grp_ids) <- names(grp_specs)

  grp_rows <- lapply(seq_along(grp_specs), function(k) {
    spec <- grp_specs[[k]]
    gid <- unname(grp_ids[k])
    if (is.null(spec$levels)) {
      data.frame(
        id = gid, name = spec$variable, label = NA_character_,
        groupingDataset = spec$dataset, groupingVariable = spec$variable,
        dataDriven = "TRUE",
        group_id = NA_character_, group_name = NA_character_,
        group_label = NA_character_, group_level = NA_integer_,
        group_order = NA_integer_,
        group_compoundExpression_logicalOperator = NA_character_,
        group_condition_dataset = NA_character_,
        group_condition_variable = NA_character_,
        group_condition_comparator = NA_character_,
        group_condition_value = NA_character_,
        stringsAsFactors = FALSE
      )
    } else {
      n <- length(spec$levels)
      data.frame(
        id = gid, name = spec$variable, label = NA_character_,
        groupingDataset = spec$dataset, groupingVariable = spec$variable,
        dataDriven = "FALSE",
        group_id = sprintf("%s_%02d", gid, seq_len(n)),
        group_name = spec$levels,
        group_label = spec$levels, group_level = 1L,
        group_order = seq_len(n),
        group_compoundExpression_logicalOperator = NA_character_,
        group_condition_dataset = spec$dataset,
        group_condition_variable = spec$variable,
        group_condition_comparator = "EQ",
        group_condition_value = spec$levels,
        stringsAsFactors = FALSE
      )
    }
  })
  AnalysisGroupings <- do.call(rbind, grp_rows)

  # --- Data subsets (deduplicated AND chains) --------------------------------
  ds_specs <- character(0)   # signature -> id, names(ds_specs) = signature
  ana_ds_id <- rep(NA_character_, nrow(analyses))
  ds_rows <- list()
  for (i in seq_len(nrow(analyses))) {
    w <- analyses$where[i]
    if (.ars_blank(w)) next
    cond <- .ars_parse_condition_chain(w, default_dataset = analyses$dataset[i])
    sig <- paste(w, analyses$dataset[i], sep = "\n")
    if (!sig %in% names(ds_specs)) {
      dss_id <- sprintf("Dss_%02d", length(ds_specs) + 1L)
      ds_specs[[sig]] <- dss_id
      if (nrow(cond) == 1) {
        ds_rows[[dss_id]] <- data.frame(
          id = dss_id, name = w, description = NA_character_, label = w,
          level = 1L, order = 1L,
          compoundExpression_logicalOperator = NA_character_,
          condition_dataset = cond$dataset,
          condition_variable = cond$variable,
          condition_comparator = cond$comparator,
          condition_value = cond$value,
          stringsAsFactors = FALSE
        )
      } else {
        ds_rows[[dss_id]] <- rbind(
          data.frame(
            id = dss_id, name = w, description = NA_character_, label = w,
            level = 1L, order = 1L,
            compoundExpression_logicalOperator = "AND",
            condition_dataset = NA_character_,
            condition_variable = NA_character_,
            condition_comparator = NA_character_,
            condition_value = NA_character_,
            stringsAsFactors = FALSE
          ),
          data.frame(
            id = dss_id, name = w, description = NA_character_, label = w,
            level = 2L, order = seq_len(nrow(cond)),
            compoundExpression_logicalOperator = NA_character_,
            condition_dataset = cond$dataset,
            condition_variable = cond$variable,
            condition_comparator = cond$comparator,
            condition_value = cond$value,
            stringsAsFactors = FALSE
          )
        )
      }
    }
    ana_ds_id[i] <- ds_specs[[sig]]
  }
  DataSubsets <- if (length(ds_rows) > 0) {
    do.call(rbind, ds_rows)
  } else {
    data.frame(
      id = character(0), name = character(0), description = character(0),
      label = character(0), level = integer(0), order = integer(0),
      compoundExpression_logicalOperator = character(0),
      condition_dataset = character(0), condition_variable = character(0),
      condition_comparator = character(0), condition_value = character(0),
      stringsAsFactors = FALSE
    )
  }

  # --- Methods (expanded from the vendored siera catalog) --------------------
  # Resolve the catalog key per analysis: a flat subject-count categorical
  # (single grouping, variable USUBJID) is generated from the dedicated
  # branch-free flat template so the emitted programme stays readable --
  # the compact parameter format keeps saying "categorical_summary".
  analyses$.method_key <- analyses$method
  flat_sel <- analyses$method == "categorical_summary" &
    vapply(ana_grp_ids, length, integer(1)) == 1L &
    toupper(analyses$variable) == "USUBJID" &
    "categorical_summary_flat" %in% names(catalog)
  analyses$.method_key[flat_sel] <- "categorical_summary_flat"

  # proportion_ci carries per-analysis options (CI method, confidence level,
  # optional single response level).  siera resolves template parameters only
  # from its own computed value sources, so each distinct option set is baked
  # into its own synthesized method entry with a readable id (issue #40).
  bad_opt <- !is.na(analyses$options) & analyses$method != "proportion_ci"
  if (any(bad_opt)) {
    i <- which(bad_opt)[1]
    stop("`analyses` row ", i, " (", analyses$analysis_id[i],
         "): `options` is only supported for method 'proportion_ci' (got '",
         analyses$method[i], "').", call. = FALSE)
  }
  for (i in which(analyses$method == "proportion_ci")) {
    opt <- .ars_parse_options(analyses$options[i])
    extra <- setdiff(names(opt), c("method", "conf.level", "value"))
    if (length(extra) > 0) {
      stop("`analyses` row ", i, " (", analyses$analysis_id[i],
           "): unknown `options` key(s) for proportion_ci: ",
           paste(extra, collapse = ", "),
           ". Supported: method, conf.level, value.", call. = FALSE)
    }
    ci_method <- if ("method" %in% names(opt)) opt[["method"]] else
      "clopper-pearson"
    conf <- suppressWarnings(as.numeric(
      if ("conf.level" %in% names(opt)) opt[["conf.level"]] else "0.95"))
    if (is.na(conf) || conf <= 0 || conf >= 1) {
      stop("`analyses` row ", i, " (", analyses$analysis_id[i],
           "): `options` conf.level must be a number in (0, 1).",
           call. = FALSE)
    }
    value <- if ("value" %in% names(opt)) opt[["value"]] else NULL
    key <- paste0("proportion_ci_",
                  gsub("[^a-z0-9]+", "_", tolower(ci_method)), "_",
                  round(conf * 100),
                  if (!is.null(value)) {
                    paste0("_", gsub("[^A-Za-z0-9]+", "_", value))
                  })
    if (is.null(catalog[[key]])) {
      entry <- catalog[["proportion_ci"]]
      tc <- .ars_chr(entry$templateCode)
      tc <- gsub("civaluearghere",
                 if (is.null(value)) "" else {
                   paste0(",\n    value = list(anavarhere = '", value, "')")
                 },
                 tc, fixed = TRUE)
      tc <- gsub("cimethodhere", ci_method, tc, fixed = TRUE)
      tc <- gsub("conflevelhere", format(conf, trim = TRUE), tc, fixed = TRUE)
      entry$templateCode <- tc
      entry$id <- key
      entry$name <- paste0(.ars_chr(entry$name), " (", ci_method, ", ",
                           format(conf * 100, trim = TRUE), "%",
                           if (!is.null(value)) paste0(", level '", value, "'"),
                           ")")
      catalog[[key]] <- entry
    }
    analyses$.method_key[i] <- key
  }

  used_keys <- unique(analyses$.method_key)
  method_id_of <- paste0("Mth_", used_keys)
  names(method_id_of) <- used_keys
  context <- attr(catalog, "context")

  am_rows <- list(); tpl_rows <- list(); par_rows <- list()
  for (key in used_keys) {
    m <- catalog[[key]]
    mid <- unname(method_id_of[key])
    ops <- m$operations
    am_rows[[key]] <- data.frame(
      id = mid,
      name = .ars_chr(m$name), label = .ars_chr(m$label),
      description = .ars_chr(m$description),
      operation_id = vapply(ops, function(o) {
        paste0(mid, "_op", o$order)
      }, character(1)),
      operation_name = vapply(ops, function(o) .ars_chr(o$name), character(1)),
      operation_order = vapply(ops, function(o) as.integer(o$order), integer(1)),
      operation_label = vapply(ops, function(o) .ars_chr(o$label), character(1)),
      operation_resultPattern = vapply(ops, function(o) {
        .ars_chr(o$resultPattern)
      }, character(1)),
      stringsAsFactors = FALSE
    )
    tpl_rows[[key]] <- data.frame(
      method_id = mid,
      context = if (!is.null(m$context)) .ars_chr(m$context) else context,
      specifiedAs = "Code",
      templateCode = gsub("\r", "", .ars_chr(m$templateCode), fixed = TRUE),
      stringsAsFactors = FALSE
    )
    pars <- m$parameters
    par_rows[[key]] <- data.frame(
      method_id = mid,
      parameter_name = vapply(pars, function(p) .ars_chr(p$name), character(1)),
      parameter_description = vapply(pars, function(p) {
        .ars_chr(p$description)
      }, character(1)),
      parameter_label = vapply(pars, function(p) .ars_chr(p$label), character(1)),
      parameter_valueSource = vapply(pars, function(p) {
        .ars_chr(p$valueSource)
      }, character(1)),
      parameter_value = NA_character_,
      stringsAsFactors = FALSE
    )
  }
  AnalysisMethods <- do.call(rbind, am_rows)
  AnalysisMethodCodeTemplate <- do.call(rbind, tpl_rows)
  AnalysisMethodCodeParameters <- do.call(rbind, par_rows)
  row.names(AnalysisMethods) <- NULL
  row.names(AnalysisMethodCodeTemplate) <- NULL
  row.names(AnalysisMethodCodeParameters) <- NULL

  # Which methods reference a denominator analysis?
  needs_denom <- vapply(used_keys, function(key) {
    any(vapply(catalog[[key]]$parameters, function(p) {
      identical(p$valueSource, "DEN_analysisid")
    }, logical(1)))
  }, logical(1))
  names(needs_denom) <- used_keys

  # --- Denominator wiring -----------------------------------------------------
  grp1_of <- vapply(ana_grp_ids, function(ids) {
    unname(grp_ids[ids[1]])
  }, character(1))
  den_of <- rep(NA_character_, nrow(analyses))
  for (i in seq_len(nrow(analyses))) {
    if (!needs_denom[[analyses$.method_key[i]]]) next
    den <- analyses$denominator[i]
    if (.ars_blank(den) || identical(trimws(den), "auto")) {
      cand <- which(
        analyses$output_id == analyses$output_id[i] &
          analyses$method == "total_n" &
          analyses$.anset_id == analyses$.anset_id[i] &
          grp1_of == grp1_of[i]
      )
      if (length(cand) != 1) {
        stop("`analyses` row ", i, " (", analyses$analysis_id[i],
             "): cannot auto-resolve `denominator` (need exactly one ",
             "'total_n' analysis with the same population and first grouping ",
             "in output '", analyses$output_id[i], "'; found ",
             length(cand), "). Set `denominator` explicitly.", call. = FALSE)
      }
      den_of[i] <- analyses$analysis_id[cand]
    } else {
      den <- trimws(den)
      if (!den %in% analyses$analysis_id) {
        stop("`analyses` row ", i, " (", analyses$analysis_id[i],
             "): `denominator` references unknown analysis_id '", den, "'.",
             call. = FALSE)
      }
      den_of[i] <- den
    }
    # siera's generated programme computes each analysis in row order and a
    # percentage analysis reads its denominator's data frame (df2_<id>), so
    # a denominator listed AFTER its referent fails at run time with
    # "object 'df2_<id>' not found".
    if (match(den_of[i], analyses$analysis_id) > i) {
      stop("`analyses` row ", i, " (", analyses$analysis_id[i],
           "): its denominator analysis '", den_of[i], "' comes AFTER it. ",
           "siera generates the programme in row order, so the total_n ",
           "denominator must be listed before the analyses that reference ",
           "it - move it up (cards ARDs list .total_n/.by_stats counts ",
           "last; ars_params_from_ard() already reorders them).",
           call. = FALSE)
    }
  }

  # --- Analyses sheet ---------------------------------------------------------
  max_grp <- max(vapply(ana_grp_ids, length, integer(1)))
  auto_name <- function(i) {
    gb_vars <- .ars_parse_group_by(analyses$group_by[i])$variable
    nm <- paste0("Summary of ", analyses$dataset[i], ".",
                 analyses$variable[i], " by ",
                 paste(gb_vars, collapse = " and "))
    if (!.ars_blank(analyses$where[i])) {
      nm <- paste0(nm, " (", analyses$where[i], ")")
    }
    nm
  }
  Analyses <- data.frame(
    id = analyses$analysis_id,
    version = 1L,
    name = vapply(seq_len(nrow(analyses)), function(i) {
      if (.ars_blank(analyses$name[i])) auto_name(i) else analyses$name[i]
    }, character(1)),
    categoryIds = NA_character_,
    reason = "SPECIFIED IN SAP",
    purpose = NA_character_,
    analysisSetId = analyses$.anset_id,
    stringsAsFactors = FALSE
  )
  for (g in seq_len(max_grp)) {
    Analyses[[paste0("groupingId", g)]] <- vapply(ana_grp_ids, function(ids) {
      if (length(ids) >= g) unname(grp_ids[ids[g]]) else NA_character_
    }, character(1))
    Analyses[[paste0("resultsByGroup", g)]] <- ifelse(
      is.na(Analyses[[paste0("groupingId", g)]]), NA, TRUE
    )
  }
  Analyses$dataSubsetId <- ana_ds_id
  Analyses$dataset <- analyses$dataset
  Analyses$variable <- analyses$variable
  Analyses$method_id <- unname(method_id_of[analyses$.method_key])
  has_den <- !is.na(den_of)
  Analyses$referencedAnalysisOperations_referencedOperationId1 <- ifelse(
    has_den, paste0(Analyses$method_id, "_pct_num"), NA_character_
  )
  Analyses$referencedAnalysisOperations_analysisId1 <- ifelse(
    has_den, analyses$analysis_id, NA_character_
  )
  Analyses$referencedAnalysisOperations_referencedOperationId2 <- ifelse(
    has_den, paste0(Analyses$method_id, "_pct_den"), NA_character_
  )
  Analyses$referencedAnalysisOperations_analysisId2 <- den_of

  # --- Contents lists ---------------------------------------------------------
  lopo <- data.frame(
    name = "List of Planned Outputs", label = "LOPO",
    listItem_level = 1L,
    listItem_name = outputs$name,
    listItem_order = seq_len(nrow(outputs)),
    listItem_analysisId = NA_character_,
    listItem_outputId = outputs$output_id,
    stringsAsFactors = FALSE
  )
  main_rows <- list()
  for (o in seq_len(nrow(outputs))) {
    oid <- outputs$output_id[o]
    main_rows[[length(main_rows) + 1L]] <- data.frame(
      name = "List of Planned Analyses", label = "LOPA",
      listItem_level = 1L, listItem_name = outputs$name[o],
      listItem_order = o,
      listItem_analysisId = NA_character_, listItem_outputId = oid,
      stringsAsFactors = FALSE
    )
    sel <- which(analyses$output_id == oid)
    main_rows[[length(main_rows) + 1L]] <- data.frame(
      name = "List of Planned Analyses", label = "LOPA",
      listItem_level = 2L,
      listItem_name = Analyses$name[sel],
      listItem_order = seq_along(sel),
      listItem_analysisId = analyses$analysis_id[sel],
      listItem_outputId = NA_character_,
      stringsAsFactors = FALSE
    )
  }
  MainListOfContents <- do.call(rbind, main_rows)
  row.names(MainListOfContents) <- NULL

  # --- Provenance sheets ------------------------------------------------------
  study <- params$study
  get_study <- function(key) {
    if (is.null(study) || !all(c("key", "value") %in% names(study))) {
      return(NA_character_)
    }
    hit <- study$value[study$key == key]
    if (length(hit) == 0) NA_character_ else hit[1]
  }
  About <- data.frame(
    generatedUsing = paste0("ydisctools ",
                            getNamespaceVersion("ydisctools"),
                            " build_ars()"),
    version = "1",
    note = "ARS metadata generated from a compact parameter set.",
    stringsAsFactors = FALSE
  )
  ReportingEvent <- data.frame(
    id = ifelse(is.na(get_study("study_id")), "RE_01",
                paste0("RE_", get_study("study_id"))),
    name = ifelse(is.na(get_study("study_title")),
                  "Reporting Event", get_study("study_title")),
    stringsAsFactors = FALSE
  )

  # siera's .generate_adam_loading_code() collects the datasets to read as
  # unique(c(Analyses$dataset, AnalysisSets$condition_dataset,
  # DataSubsets$condition_dataset)) CASE-SENSITIVELY, so one logical dataset
  # spelled two ways (e.g. the user's `adsl` in `dataset` vs the "ADSL"
  # default a bare-flag population bakes in) becomes two reads of the same
  # file. Fold every dataset reference to one spelling per case-insensitive
  # key, preferring the user-authored Analyses$dataset spelling.
  ds_canon <- .ars_canonicalize_datasets(Analyses, AnalysisSets, DataSubsets)
  Analyses <- ds_canon$Analyses
  AnalysisSets <- ds_canon$AnalysisSets
  DataSubsets <- ds_canon$DataSubsets

  # --- Display metadata (optional Displays params sheet) ----------------------
  disp <- .ars_display_sheets(params$displays, outputs)

  out <- list(
    About = About,
    ReportingEvent = ReportingEvent,
    MainListOfContents = MainListOfContents,
    OtherListsOfContents = lopo
  )
  if (!is.null(disp$Displays)) {
    out$GlobalDisplaySections <- disp$GlobalDisplaySections
  }
  out$Outputs <- disp$Outputs
  if (!is.null(disp$Displays)) {
    out$Displays <- disp$Displays
  }
  c(out, list(
    DataSubsets = DataSubsets,
    AnalysisSets = AnalysisSets,
    AnalysisGroupings = AnalysisGroupings,
    Analyses = Analyses,
    AnalysisMethods = AnalysisMethods,
    AnalysisMethodCodeTemplate = AnalysisMethodCodeTemplate,
    AnalysisMethodCodeParameters = AnalysisMethodCodeParameters
  ))
}

# Build the ARS Outputs sheet (always) and -- when the optional compact
# `Displays` parameter sheet (output_id / section_type / order / text) is
# present -- the Displays + GlobalDisplaySections sheets, laid out one row per
# display subsection as in siera's bundled example workbooks.
.ars_display_sheets <- function(displays, outputs) {
  allowed <- c("Header", "Title", "Footnote", "Footer", "Abbreviations",
               "Rowlabel Header")
  has_disp <- !is.null(displays) && nrow(as.data.frame(displays)) > 0

  disp_id_of <- function(oid) paste0("Disp_", oid)

  if (has_disp) {
    displays <- as.data.frame(displays, stringsAsFactors = FALSE)
    need <- c("output_id", "section_type", "order", "text")
    missing_cols <- setdiff(need, names(displays))
    if (length(missing_cols) > 0) {
      stop("`displays` is missing column(s): ",
           paste(missing_cols, collapse = ", "), ".", call. = FALSE)
    }
    bad_out <- setdiff(displays$output_id, outputs$output_id)
    if (length(bad_out) > 0) {
      stop("`displays$output_id` values not present in `outputs`: ",
           paste(unique(bad_out), collapse = ", "), ".", call. = FALSE)
    }
    bad_type <- setdiff(displays$section_type, allowed)
    if (length(bad_type) > 0) {
      stop("`displays$section_type` must be one of ",
           paste(allowed, collapse = ", "), " (got: ",
           paste(unique(bad_type), collapse = ", "), ").", call. = FALSE)
    }
  }

  Outputs <- data.frame(
    id = outputs$output_id,
    version = 1L,
    name = outputs$name,
    categoryIds = NA_character_,
    display1_Id = if (has_disp) {
      ifelse(outputs$output_id %in% displays$output_id,
             disp_id_of(outputs$output_id), NA_character_)
    } else {
      NA_character_
    },
    fileSpecification_file_name = NA_character_,
    fileSpecification_file_location = NA_character_,
    stringsAsFactors = FALSE
  )

  if (!has_disp) {
    return(list(Outputs = Outputs, Displays = NULL,
                GlobalDisplaySections = NULL))
  }

  rows <- list()
  for (oid in unique(displays$output_id)) {
    d <- displays[displays$output_id == oid, , drop = FALSE]
    d$order <- suppressWarnings(as.integer(d$order))
    d <- d[order(match(d$section_type, allowed), d$order), , drop = FALSE]
    oname <- outputs$name[outputs$output_id == oid][1]
    titles <- d$text[d$section_type == "Title"]
    disp_title <- if (length(titles) > 0) titles[length(titles)] else oname
    type_key <- gsub(" ", "", d$section_type, fixed = TRUE)
    rows[[oid]] <- data.frame(
      id = disp_id_of(oid),
      name = oname,
      version = 1L,
      displayTitle = disp_title,
      displaySection_sectionType = d$section_type,
      displaySection_orderedSubSection_order = d$order,
      displaySection_subSection_id = sprintf("%s_%s_%02d", disp_id_of(oid),
                                             type_key, d$order),
      displaySection_subSection_text = d$text,
      stringsAsFactors = FALSE
    )
  }
  Displays <- do.call(rbind, rows)
  row.names(Displays) <- NULL

  GlobalDisplaySections <- data.frame(
    sectionType = character(0), subSection_id = character(0),
    subSection_text = character(0), stringsAsFactors = FALSE
  )
  list(Outputs = Outputs, Displays = Displays,
       GlobalDisplaySections = GlobalDisplaySections)
}

# -- write_ars_xlsx() ---------------------------------------------------------

#' Write ARS metadata tables to a siera-readable workbook
#'
#' Writes the list of ARS sheet tables produced by [build_ars()] to an
#' \code{.xlsx} workbook that \code{siera::readARS()} accepts directly.  For a
#' JSON ARS, convert the written workbook with
#' \code{siera::ars_xlsx_to_json()} (available in siera > 0.5.6).
#'
#' @param ars Named list of data frames as returned by [build_ars()].
#' @param path File path of the workbook to write (\code{.xlsx}).
#' @param overwrite Logical; overwrite an existing file? Default \code{FALSE}.
#'
#' @return The path, invisibly.
#'
#' @seealso [build_ars()], [ars_param_template()]
#'
#' @examples
#' tmp_params <- file.path(tempdir(), "ars_params.xlsx")
#' ars_param_template(tmp_params, overwrite = TRUE)
#' tmp_ars <- file.path(tempdir(), "ARS_STUDY01.xlsx")
#' write_ars_xlsx(build_ars(tmp_params), tmp_ars, overwrite = TRUE)
#' \dontrun{
#' siera::readARS(tmp_ars, output_path = tempdir(), adam_path = "path/to/adam")
#' }
#'
#' @export
write_ars_xlsx <- function(ars, path, overwrite = FALSE) {
  if (!is.list(ars) || is.null(names(ars)) ||
      !all(vapply(ars, is.data.frame, logical(1)))) {
    stop("`ars` must be a named list of data frames (see build_ars()).",
         call. = FALSE)
  }
  required <- c("MainListOfContents", "OtherListsOfContents", "DataSubsets",
                "AnalysisSets", "AnalysisGroupings", "Analyses",
                "AnalysisMethods", "AnalysisMethodCodeTemplate",
                "AnalysisMethodCodeParameters")
  missing <- setdiff(required, names(ars))
  if (length(missing) > 0) {
    stop("`ars` is missing sheet(s) required by siera::readARS(): ",
         paste(missing, collapse = ", "), ".", call. = FALSE)
  }
  if (file.exists(path) && !overwrite) {
    stop("File already exists: '", path, "'. Set overwrite = TRUE to replace.",
         call. = FALSE)
  }
  writexl::write_xlsx(ars, path)
  invisible(path)
}
