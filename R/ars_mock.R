# ============================================================================
#  ars_mock.R -- mock shells (xlsx / rtf) from the TFL TOC workbook
# ============================================================================

#' Generate TFL mock shells from a TOC workbook
#'
#' Builds mock (shell) displays from the same TOC workbook that
#' [ars_from_toc()] expands into ARS metadata -- the shells and the ARS are
#' derived from the same analyses, so they cannot drift apart.  Each display
#' gets a title block (\code{"Table <toc_no>"}, title, population label), arm
#' columns with \code{(N=XXX)} placeholders, and stub rows per analysis:
#' continuous methods emit the n / Mean (SD) / Median / Q1, Q3 / Min, Max
#' block, categorical methods emit indented category placeholder rows, and
#' \code{total_n} feeds the header \code{N=XXX} only.
#'
#' Two formats:
#' \describe{
#'   \item{\code{"xlsx"}}{one sheet per display, written with \pkg{openxlsx}
#'     (title rows + bold column header).}
#'   \item{\code{"rtf"}}{ONE document with one display per page, written with
#'     \pkg{rtfreporter} -- the mock carries the same furniture the final
#'     rtfreporter table will.}
#' }
#' Both packages are optional (Suggests) and checked at run time.
#'
#' @param toc TOC workbook path or list, as for [ars_from_toc()].
#' @param path Output file path (\code{.xlsx} or \code{.rtf}, matching
#'   \code{format}).
#' @param format \code{"xlsx"} (default) or \code{"rtf"}.
#' @param section_map Passed to [ars_from_toc()] (numbering).
#' @param overwrite Logical; overwrite an existing file? Default \code{FALSE}.
#'
#' @return The path, invisibly.
#'
#' @seealso [ars_toc_template()], [ars_from_toc()]
#'
#' @examples
#' tmp_toc <- file.path(tempdir(), "toc.xlsx")
#' ars_toc_template(tmp_toc, overwrite = TRUE)
#' if (requireNamespace("openxlsx", quietly = TRUE)) {
#'   mock <- file.path(tempdir(), "mocks.xlsx")
#'   ars_mock(tmp_toc, mock, overwrite = TRUE)
#' }
#'
#' @export
ars_mock <- function(toc, path, format = c("xlsx", "rtf"),
                     section_map = NULL, overwrite = FALSE) {
  format <- match.arg(format)
  if (file.exists(path) && !overwrite) {
    stop("File already exists: '", path, "'. Set overwrite = TRUE to replace.",
         call. = FALSE)
  }
  params <- ars_from_toc(toc, section_map = section_map)
  shells <- lapply(seq_len(nrow(params$toc)), function(i) {
    .ars_mock_shell(params$toc[i, ], params$analyses, params$outputs)
  })
  names(shells) <- params$toc$output_id

  if (format == "xlsx") {
    .ars_mock_write_xlsx(shells, path)
  } else {
    .ars_mock_write_rtf(shells, path)
  }
  invisible(path)
}

# -- shell construction --------------------------------------------------------

# Placeholder text per statistic row of each method's mock block.
.ars_mock_rows_for <- function(method, name, group2 = NA_character_) {
  ind <- "  "
  if (method == "continuous_summary") {
    return(data.frame(
      stub = c(name, paste0(ind, c("n", "Mean (SD)", "Median", "Q1, Q3",
                                   "Min, Max"))),
      value = c("", "XX", "XX.X (XX.XX)", "XX.X", "XX.X, XX.X", "XX, XX"),
      stringsAsFactors = FALSE
    ))
  }
  if (method == "categorical_summary") {
    if (!is.na(group2)) {
      var <- sub("^.*\\.", "", group2)   # strip a dataset qualifier
      return(data.frame(
        stub = c(name, paste0(ind, "<", var, " level ", 1:2, ">")),
        value = c("", "XX (XX.X)", "XX (XX.X)"),
        stringsAsFactors = FALSE
      ))
    }
    return(data.frame(stub = name, value = "XX (XX.X)",
                      stringsAsFactors = FALSE))
  }
  # tests and other methods: one result row
  data.frame(stub = name, value = "X.XXX", stringsAsFactors = FALSE)
}

# Build one display's shell: list(titles, header, body) where body is a
# data.frame stub + one column per arm.
.ars_mock_shell <- function(toc_row, analyses, outputs) {
  an <- analyses[analyses$output_id == toc_row$output_id, , drop = FALSE]

  # arm columns from pre-defined groups, else generic placeholders
  groups <- .ars_parse_groups(
    if (is.na(toc_row$groups)) "" else toc_row$groups)
  arms <- if (is.null(groups)) {
    paste0("<", toc_row$group_by, " level ", 1:2, ">")
  } else {
    groups
  }
  arm_cols <- paste0(arms, "\n(N=XXX)")

  blocks <- list()
  for (j in seq_len(nrow(an))) {
    if (an$method[j] == "total_n") next   # big N lives in the column header
    grp <- an$group_by[j]
    grp2 <- if (!is.na(grp) && grepl(",", grp, fixed = TRUE)) {
      trimws(strsplit(grp, ",", fixed = TRUE)[[1]][2])
    } else {
      NA_character_
    }
    blocks[[length(blocks) + 1L]] <-
      .ars_mock_rows_for(an$method[j], an$name[j], grp2)
  }
  rows <- if (length(blocks) > 0) {
    do.call(rbind, blocks)
  } else {
    data.frame(stub = character(0), value = character(0),
               stringsAsFactors = FALSE)
  }

  catalog <- .ars_recipe_catalog()
  stub_label <- if (toc_row$display_type %in% names(catalog)) {
    catalog[[toc_row$display_type]]$stub_label
  } else {
    "Parameter"
  }

  body <- data.frame(stub = rows$stub, stringsAsFactors = FALSE)
  for (a in arm_cols) body[[a]] <- ifelse(rows$value == "", "", rows$value)
  names(body)[1] <- stub_label

  pop <- .ars_parse_condition_chain(toc_row$population,
                                    default_dataset = "ADSL")
  titles <- c(paste0("Table ", toc_row$toc_no),
              toc_row$title,
              .ars_population_name(toc_row$population, pop))
  list(titles = titles, body = body)
}

# -- writers -------------------------------------------------------------------

.ars_mock_write_xlsx <- function(shells, path) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("ars_mock(format = \"xlsx\") needs the openxlsx package.",
         call. = FALSE)
  }
  wb <- openxlsx::createWorkbook()
  bold <- openxlsx::createStyle(textDecoration = "bold")
  header <- openxlsx::createStyle(textDecoration = "bold", wrapText = TRUE,
                                  border = "TopBottom")
  for (nm in names(shells)) {
    sh <- shells[[nm]]
    sheet <- substr(nm, 1, 31)
    openxlsx::addWorksheet(wb, sheet)
    for (t in seq_along(sh$titles)) {
      openxlsx::writeData(wb, sheet, sh$titles[t], startRow = t)
    }
    openxlsx::addStyle(wb, sheet, bold, rows = 1, cols = 1)
    start <- length(sh$titles) + 2L
    body <- sh$body
    names(body) <- gsub("\n", " ", names(body), fixed = TRUE)
    openxlsx::writeData(wb, sheet, body, startRow = start,
                        headerStyle = header)
    openxlsx::setColWidths(wb, sheet, cols = seq_len(ncol(body)),
                           widths = c(40, rep(18, ncol(body) - 1L)))
  }
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
}

.ars_mock_write_rtf <- function(shells, path) {
  if (!requireNamespace("rtfreporter", quietly = TRUE)) {
    stop("ars_mock(format = \"rtf\") needs the rtfreporter package ",
         "(https://github.com/ichirio/rtfreporter).", call. = FALSE)
  }
  pages <- lapply(shells, function(sh) {
    rtfreporter::rtftable(
      sh$body,
      col_header = names(sh$body),
      row_title = 1L
    )
  })
  titles <- lapply(shells, function(sh) as.list(sh$titles))
  doc <- rtfreporter::rtf_document(
    page = list(paper_size = "letter", orientation = "landscape")
  )
  doc <- rtfreporter::rtf_section(doc, page = 1,
                                  secinfo = list(header = NULL, footer = NULL))
  doc <- rtfreporter::rtf_tables(doc, unname(pages), titles = unname(titles))
  rtfreporter::generate_rtfreport(doc, path, overwrite = TRUE)
}
