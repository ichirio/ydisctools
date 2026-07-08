# ============================================================================
#  ars_run.R -- run siera-generated ARD programmes and combine the ARDs
# ============================================================================

# ADaM formats read_adam() understands, in the priority order it picks when
# several same-named files coexist in one folder.
.ADAM_FORMATS <- c("xpt", "sas7bdat", "rds", "rda", "rdata", "csv")

#' Read an ADaM dataset by format priority
#'
#' Reads one ADaM dataset from a \emph{stem} (a path without extension),
#' picking the actual file present on disk by format priority when several
#' same-named files coexist: \strong{\code{.xpt} > \code{.sas7bdat} >
#' \code{.rds} > \code{.rda}/\code{.rdata} > \code{.csv}}.  This is what
#' [ars_generate_ard()] wires the siera-generated programmes to call, so a
#' study's real transport/SAS/R datasets are read directly instead of
#' siera's hardcoded \code{.csv} (siera 0.5.6 only emits
#' \code{readr::read_csv()}).
#'
#' The readers: \code{.xpt} via \code{haven::read_xpt()}, \code{.sas7bdat}
#' via \code{haven::read_sas()}, \code{.rds} via [readRDS()], \code{.rda} /
#' \code{.rdata} via [load()] (the object named after the stem, else the
#' first loaded object), \code{.csv} via \code{readr::read_csv()}.  For the
#' \pkg{haven} formats value labels are zapped (\code{haven::zap_labels()})
#' so a \code{haven_labelled} column behaves like the plain vector the
#' \code{.csv} route would yield.
#'
#' @param path Dataset stem (e.g. \code{"data/ADSL"}) or a full file name;
#'   any recognised extension is stripped and the folder is searched by
#'   priority.  Matching is case-insensitive on the extension and the stem.
#'
#' @return A data frame (tibble) with the dataset contents.
#'
#' @seealso [ars_generate_ard()]
#'
#' @examples
#' \dontrun{
#' # reads data/ADSL.xpt if present, else .sas7bdat, .rds, .rda, .csv
#' adsl <- read_adam("data/ADSL")
#' }
#'
#' @export
read_adam <- function(path) {
  if (!is.character(path) || length(path) != 1 || is.na(path)) {
    stop("`path` must be a single dataset stem or file name.", call. = FALSE)
  }
  stem <- sub("[.](xpt|sas7bdat|rds|rda|rdata|csv)$", "", path,
              ignore.case = TRUE)
  dir <- dirname(stem)
  base <- basename(stem)

  # resolve the file: exact stem.ext first, then a case-insensitive scan of
  # the folder (real ADaM libraries are often lowercase adsl.xpt)
  found <- NA_character_
  fmt <- NA_character_
  for (e in .ADAM_FORMATS) {
    cand <- paste0(stem, ".", e)
    if (file.exists(cand)) { found <- cand; fmt <- e; break }
  }
  if (is.na(found) && dir.exists(dir)) {
    fs <- list.files(dir, full.names = TRUE)
    lc <- tolower(basename(fs))
    for (e in .ADAM_FORMATS) {
      hit <- fs[lc == tolower(paste0(base, ".", e))]
      if (length(hit) > 0) { found <- hit[1]; fmt <- e; break }
    }
  }
  if (is.na(found)) {
    stop("No ADaM file found for '", stem, "' (looked for '", base,
         "' with extension ", paste0(".", .ADAM_FORMATS, collapse = " / "),
         " in '", dir, "').", call. = FALSE)
  }

  df <- switch(
    fmt,
    xpt = haven::zap_labels(haven::read_xpt(found)),
    sas7bdat = haven::zap_labels(haven::read_sas(found)),
    rds = readRDS(found),
    rda = ,
    rdata = {
      e <- new.env()
      nms <- load(found, envir = e)
      pick <- if (base %in% nms) base else
        if (tolower(base) %in% tolower(nms)) nms[tolower(nms) == tolower(base)][1]
        else nms[1]
      get(pick, envir = e)
    },
    csv = readr::read_csv(found, show_col_types = FALSE, progress = FALSE)
  )
  if (!is.data.frame(df)) {
    stop("The ", fmt, " file '", found, "' did not contain a data frame.",
         call. = FALSE)
  }
  df
}

# siera 0.5.6 bakes `readr::read_csv('<adam_path>/<DATASET>.csv',
# show_col_types = FALSE, progress = FALSE)` into every generated programme.
# Rewrite that reader head to `ydisctools::read_adam('<adam_path>/<DATASET>')`
# so the actual file present (any supported format) is read at run time; the
# trailing `|> dplyr::mutate(...)` blank-fill pipe is left untouched.
.ars_csv_to_read_adam <- function(code) {
  gsub("readr::read_csv\\('([^']*)\\.csv'[^)]*\\)",
       "ydisctools::read_adam('\\1')", code, perl = TRUE)
}

#' Generate and run ARD programmes, returning one combined ARD
#'
#' A thin driver around \code{siera::readARS()}: generates the per-output ARD
#' programmes from an ARS metadata file (or directly from the sheet list
#' returned by [build_ars()]), runs each programme in an isolated
#' environment, and returns the Analysis Results Datasets bound into a single
#' data frame.  siera stamps \code{AnalysisId} / \code{MethodId} /
#' \code{OutputId} onto every ARD row, so the combined ARD is keyed by output
#' out of the box.
#'
#' The generated programmes open with \code{library()} calls for siera's
#' runtime stack; because every generated statement is namespace-qualified,
#' those lines are neutralised before sourcing, so running the programmes
#' does not attach packages to the caller's search path.  The packages the
#' generated code actually calls (\pkg{cards}, \pkg{dplyr}, \pkg{readr}, and
#' \pkg{cardx} for test methods) must be installed.
#'
#' \strong{Reading the ADaM data.}  siera 0.5.6 hardcodes
#' \code{readr::read_csv('<adam_path>/<DATASET>.csv')} in every programme.
#' This wrapper rewrites that reader to [read_adam()], so the actual file
#' present in \code{adam_path} is read by format priority
#' (\code{.xpt} > \code{.sas7bdat} > \code{.rds} > \code{.rda} >
#' \code{.csv}) -- real transport / SAS / R datasets work without a manual
#' conversion to csv.
#'
#' \strong{Generating without data.}  siera bakes only a \emph{path} into
#' the code at generation time; the data are read when the programmes
#' \emph{run}.  With \code{run = FALSE} the programmes are generated and
#' the reader is rewritten, but nothing is executed and \code{adam_path}
#' need not contain (or be) any data -- use it to obtain the R code alone.
#'
#' @param ars Path to an ARS metadata file (\code{.xlsx} or \code{.json}), or
#'   the named list of ARS sheets returned by [build_ars()] (written to a
#'   temporary workbook automatically).
#' @param adam_path Folder the generated programmes read the ADaM datasets
#'   from, in any format [read_adam()] supports (\code{.xpt} /
#'   \code{.sas7bdat} / \code{.rds} / \code{.rda} / \code{.csv}).  Required
#'   when \code{run = TRUE} (and must exist); optional when
#'   \code{run = FALSE}, where it only sets the path baked into the code
#'   (default \code{"adam"}).
#' @param output_path Folder to write the generated \code{ARD_<OutputId>.R}
#'   programmes to. Default: a fresh temporary directory.  The programmes are
#'   kept there for inspection / traceability.
#' @param spec_output Run only this output ID (passed to
#'   \code{siera::readARS()}). Default \code{""} runs every output.
#' @param combine Logical; \code{TRUE} (default) returns one combined data
#'   frame, \code{FALSE} a named list with one ARD per output.  Ignored when
#'   \code{run = FALSE}.
#' @param run Logical; \code{TRUE} (default) runs the programmes and returns
#'   the ARD(s).  \code{FALSE} only generates the programmes (no data
#'   needed) and returns their file paths invisibly.
#'
#' @return When \code{run = TRUE}: a data frame (the combined ARD) if
#'   \code{combine = TRUE}, else a named list of per-output ARD data frames;
#'   the generated programme paths are attached as attribute \code{"scripts"}.
#'   When \code{run = FALSE}: the character vector of generated programme
#'   paths, invisibly.
#'
#' @seealso [read_adam()], [build_ars()], [write_ars_xlsx()]
#'
#' @examples
#' \dontrun{
#' ex <- system.file("ars-examples", package = "ydisctools")
#' ard <- ars_generate_ard(file.path(ex, "ARS_dm_ae.xlsx"),
#'                         adam_path = file.path(ex, "adam"))
#' table(ard$OutputId)
#'
#' # code only, no data required:
#' scripts <- ars_generate_ard(file.path(ex, "ARS_dm_ae.xlsx"),
#'                             run = FALSE)
#' }
#'
#' @export
ars_generate_ard <- function(ars, adam_path = if (run) NULL else "adam",
                             output_path = tempfile("ars_ard_"),
                             spec_output = "", combine = TRUE, run = TRUE) {
  if (!requireNamespace("siera", quietly = TRUE)) {
    stop("ars_generate_ard() needs the siera package; ",
         "install it with install.packages(\"siera\").", call. = FALSE)
  }
  if (is.list(ars) && !is.data.frame(ars)) {
    ars_path <- tempfile("ARS_", fileext = ".xlsx")
    write_ars_xlsx(ars, ars_path)
  } else if (is.character(ars) && length(ars) == 1) {
    if (!file.exists(ars)) {
      stop("ARS file not found: '", ars, "'.", call. = FALSE)
    }
    ars_path <- ars
  } else {
    stop("`ars` must be an ARS file path or the sheet list from build_ars().",
         call. = FALSE)
  }
  if (run) {
    if (is.null(adam_path)) {
      stop("`adam_path` is required when run = TRUE.", call. = FALSE)
    }
    if (!dir.exists(adam_path)) {
      stop("`adam_path` folder not found: '", adam_path, "'.", call. = FALSE)
    }
  }
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }

  siera::readARS(ars_path, output_path = output_path, adam_path = adam_path,
                 spec_output = spec_output)

  scripts <- list.files(output_path, pattern = "^ARD_.*[.]R$",
                        full.names = TRUE)
  if (length(scripts) == 0) {
    stop("siera::readARS() generated no ARD programmes in '", output_path,
         "'.", call. = FALSE)
  }

  # rewrite siera's hardcoded read_csv() preamble to read_adam() (format
  # priority, resolved at run time) and persist it, so the saved programmes
  # match what actually runs
  for (s in scripts) {
    code <- paste(readLines(s, warn = FALSE), collapse = "\n")
    new_code <- .ars_csv_to_read_adam(code)
    if (!identical(code, new_code)) writeLines(new_code, s)
  }

  if (!run) {
    return(invisible(scripts))
  }

  ards <- lapply(scripts, .ars_run_script)
  names(ards) <- sub("^ARD_", "", sub("[.]R$", "", basename(scripts)))

  result <- if (combine) {
    dplyr::bind_rows(ards)
  } else {
    ards
  }
  attr(result, "scripts") <- scripts
  result
}

# Run one generated ARD programme in an isolated environment and return its
# ARD.  library() lines are neutralised: the generated statements are
# namespace-qualified, so attaching is unnecessary and would alter the
# caller's search path.
.ars_run_script <- function(path) {
  code <- gsub("^\\s*library\\(", "# library(", readLines(path))
  tf <- tempfile(fileext = ".R")
  on.exit(unlink(tf), add = TRUE)
  writeLines(code, tf)
  env <- new.env(parent = globalenv())
  source(tf, local = env)
  if (!exists("ARD", envir = env, inherits = FALSE)) {
    stop("Generated programme '", basename(path),
         "' did not produce an `ARD` object.", call. = FALSE)
  }
  get("ARD", envir = env)
}
