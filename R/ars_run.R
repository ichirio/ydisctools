# ============================================================================
#  ars_run.R -- run siera-generated ARD programmes and combine the ARDs
# ============================================================================

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
#' @param ars Path to an ARS metadata file (\code{.xlsx} or \code{.json}), or
#'   the named list of ARS sheets returned by [build_ars()] (written to a
#'   temporary workbook automatically).
#' @param adam_path Folder containing the ADaM datasets (\code{.csv} /
#'   \code{.xpt}) the programmes read.
#' @param output_path Folder to write the generated \code{ARD_<OutputId>.R}
#'   programmes to. Default: a fresh temporary directory.  The programmes are
#'   kept there for inspection / traceability.
#' @param spec_output Run only this output ID (passed to
#'   \code{siera::readARS()}). Default \code{""} runs every output.
#' @param combine Logical; \code{TRUE} (default) returns one combined data
#'   frame, \code{FALSE} a named list with one ARD per output.
#'
#' @return A data frame (the combined ARD) when \code{combine = TRUE},
#'   otherwise a named list of per-output ARD data frames.  The paths of the
#'   generated programmes are attached as attribute \code{"scripts"}.
#'
#' @seealso [build_ars()], [write_ars_xlsx()]
#'
#' @examples
#' \dontrun{
#' ex <- system.file("ars-examples", package = "ydisctools")
#' ard <- ars_generate_ard(file.path(ex, "ARS_dm_ae.xlsx"),
#'                         adam_path = file.path(ex, "adam"))
#' table(ard$OutputId)
#' }
#'
#' @export
ars_generate_ard <- function(ars, adam_path,
                             output_path = tempfile("ars_ard_"),
                             spec_output = "", combine = TRUE) {
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
  if (!dir.exists(adam_path)) {
    stop("`adam_path` folder not found: '", adam_path, "'.", call. = FALSE)
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
