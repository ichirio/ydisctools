#' Build a listing-ready table from a source data frame
#'
#' `rtf_listing()` turns a tidy source data frame into the shape a clinical
#' **listing** needs: one logical record may occupy several physical rows,
#' every cell of a record is wrapped to its column's width and padded so the
#' record's rows line up, narrow spacer columns separate the content columns,
#' and a hidden record key marks the record boundaries so the renderer can
#' paginate without ever splitting a record across pages.
#'
#' The result is an `rtflisting` object carrying the expanded data frame plus
#' the position-indexed metadata (`col_header`, `col_rel_width`, `col_spec`).
#' Hand it to [listing_to_rtftables()] to render it with
#' \pkg{rtfreporter}; this function itself only prepares data and does not
#' produce RTF.
#'
#' @section The `"type1"` template:
#' The one template implemented today reproduces the conventional G-MEDAS
#' listing style:
#'
#' 1. each display column is composed from its source columns, joined with the
#'    column's `sep` and skipping `NA` / empty values;
#' 2. each cell is wrapped to the column's `width` -- breaking after the
#'    separator first, then at word boundaries, then hard-splitting an
#'    over-long token;
#' 3. every cell of a record is padded to the record's tallest cell, so the
#'    record's lines stay aligned across columns;
#' 4. a narrow spacer column is inserted between neighbouring content columns;
#' 5. all cells are left-aligned.
#'
#' Blank separator rows between records are **not** baked in here -- they are
#' added at render time by [listing_to_rtftables()], which lets the paginator
#' count them against the page budget.
#'
#' @param data A data frame (or tibble): one row per logical record.
#' @param ... [listing_col()] specifications, in display order.
#' @param template Listing template. Currently only `"type1"`.
#' @param spacer_width Relative width of the spacer columns inserted between
#'   content columns (default `1`). `0` inserts no spacer columns at all.
#' @param record_id Name of the hidden record-key column added to the output
#'   (default `".record_id"`). It is used for grouping / pagination and is
#'   dropped before rendering.
#'
#' @return An `rtflisting` object: a list with `data` (the expanded data
#'   frame), `col_header`, `col_rel_width`, `col_spec`, `record_id`,
#'   `template` and `n_records`.
#'
#' @seealso [listing_col()] to describe a column, [suggest_listing_widths()]
#'   for starting widths, [listing_to_rtftables()] to render.
#'
#' @examples
#' adsl <- data.frame(
#'   USUBJID  = c("63016-204", "63016-205"),
#'   DISPTPD  = c("Ovarian cancer", "Ovarian cancer"),
#'   BRCA     = c("Negative", "Positive"),
#'   STAGE    = c("IIIC", "IV"),
#'   stringsAsFactors = FALSE
#' )
#'
#' lst <- rtf_listing(
#'   adsl,
#'   listing_col(USUBJID, header = "Unique\nSubject ID", width = 15),
#'   listing_col(DISPTPD, BRCA, header = "Primary Diagnosis/\nBRCA", width = 18),
#'   listing_col(STAGE,   header = "Stage at\nInitial Diagnosis", width = 10)
#' )
#' lst
#' @export
rtf_listing <- function(data, ..., template = "type1", spacer_width = 1,
                        record_id = ".record_id") {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  specs <- list(...)
  if (!length(specs) || !all(vapply(specs, inherits, logical(1L), "listing_col"))) {
    stop("`...` must be one or more listing_col() specifications.", call. = FALSE)
  }
  if (!identical(template, "type1")) {
    stop("`template` must be \"type1\" (the only template implemented).",
         call. = FALSE)
  }
  spacer_width <- as.numeric(spacer_width)
  if (length(spacer_width) != 1L || is.na(spacer_width) || spacer_width < 0) {
    stop("`spacer_width` must be a single non-negative number ",
         "(0 inserts no spacer columns).", call. = FALSE)
  }
  if (!is.character(record_id) || length(record_id) != 1L || is.na(record_id)) {
    stop("`record_id` must be a single string.", call. = FALSE)
  }
  if (record_id %in% names(data)) {
    stop(sprintf("`record_id` (\"%s\") already names a column in `data`.",
                 record_id), call. = FALSE)
  }

  names_out <- vapply(specs, function(s) s$name, character(1L))
  if (anyDuplicated(names_out)) {
    stop("listing_col() names must be unique; got: ",
         paste(names_out, collapse = ", "), call. = FALSE)
  }

  # 1. Compose each display column from its source columns.
  composed <- lapply(specs, function(s) .listing_compose(data, s))

  # Resolve each column's width: explicit, else measured from the data (so an
  # unwrapped column still gets a sensible relative width).
  widths <- vapply(seq_along(specs), function(j) {
    w <- specs[[j]]$width
    if (!is.null(w)) return(as.numeric(w))
    max(c(1L, .listing_disp_width(composed[[j]]),
          .listing_disp_width(strsplit(specs[[j]]$header, "\n", fixed = TRUE)[[1L]])))
  }, numeric(1L))

  if (nrow(data) == 0L) {
    return(.new_rtflisting(
      data          = .listing_empty_frame(names_out, record_id, spacer_width),
      specs         = specs,
      widths        = widths,
      spacer_width  = spacer_width,
      record_id     = record_id,
      template      = template,
      n_records     = 0L
    ))
  }

  # 2. Wrap every cell to its column width.
  pieces <- lapply(seq_along(specs), function(j) {
    lapply(composed[[j]], .listing_wrap,
           width = specs[[j]]$width, sep = specs[[j]]$sep)
  })

  # 3. Align each record's cells to the record's tallest cell.
  heights <- .listing_row_heights(pieces)
  pieces  <- lapply(pieces, .listing_pad, heights = heights)

  # 4. Expand to physical rows.
  out <- stats::setNames(
    lapply(pieces, function(col) unlist(col, use.names = FALSE)),
    names_out)
  out <- as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
  out[[record_id]] <- rep(seq_len(nrow(data)), times = heights)

  .new_rtflisting(
    data         = .listing_insert_spacers(out, names_out, record_id, spacer_width),
    specs        = specs,
    widths       = widths,
    spacer_width = spacer_width,
    record_id    = record_id,
    template     = template,
    n_records    = nrow(data)
  )
}

# Interleave narrow spacer columns between the content columns.  The hidden
# record-key column is kept last so it can be dropped at render time.
.listing_insert_spacers <- function(df, content, record_id, spacer_width) {
  if (spacer_width <= 0 || length(content) < 2L) {
    return(df[, c(content, record_id), drop = FALSE])
  }
  order_cols <- character(0)
  for (j in seq_along(content)) {
    if (j > 1L) {
      sp <- sprintf(".spacer%02d", j - 1L)
      df[[sp]] <- rep("", nrow(df))   # rep() so a zero-row frame stays zero-row
      order_cols <- c(order_cols, sp)
    }
    order_cols <- c(order_cols, content[[j]])
  }
  df[, c(order_cols, record_id), drop = FALSE]
}

.listing_empty_frame <- function(content, record_id, spacer_width) {
  df <- as.data.frame(
    stats::setNames(rep(list(character(0)), length(content)), content),
    stringsAsFactors = FALSE, check.names = FALSE)
  df[[record_id]] <- integer(0)
  .listing_insert_spacers(df, content, record_id, spacer_width)
}

# Assemble the rtflisting, deriving the position-indexed metadata in the
# *input* coordinates -- i.e. including the hidden record-key column, which
# as_rtftables() reindexes away when it drops that column.
.new_rtflisting <- function(data, specs, widths, spacer_width, record_id,
                            template, n_records) {
  content <- vapply(specs, function(s) s$name, character(1L))
  headers <- vapply(specs, function(s) s$header, character(1L))

  if (spacer_width > 0 && length(content) >= 2L) {
    hdr <- character(0)
    rel <- numeric(0)
    for (j in seq_along(content)) {
      if (j > 1L) {
        hdr <- c(hdr, "")
        rel <- c(rel, spacer_width)
      }
      hdr <- c(hdr, headers[[j]])
      rel <- c(rel, widths[[j]])
    }
  } else {
    hdr <- headers
    rel <- widths
  }

  # Placeholder entries for the hidden record-key column (last).
  hdr <- c(hdr, "")
  rel <- c(rel, 1)

  structure(
    list(
      data          = data,
      col_header    = hdr,
      col_rel_width = rel,
      col_spec      = lapply(seq_along(hdr),
                             function(j) list(col = j, align = "left")),
      record_id     = record_id,
      template      = template,
      n_records     = n_records
    ),
    class = "rtflisting"
  )
}

#' @export
print.rtflisting <- function(x, n = 10L, ...) {
  cat(sprintf("<rtflisting> template \"%s\": %d record%s -> %d row%s x %d column%s\n",
              x$template, x$n_records, if (x$n_records == 1L) "" else "s",
              nrow(x$data), if (nrow(x$data) == 1L) "" else "s",
              ncol(x$data), if (ncol(x$data) == 1L) "" else "s"))
  vis <- setdiff(names(x$data), x$record_id)
  cat("  Columns:    ", paste(vis, collapse = " | "), "\n", sep = "")
  cat("  Widths:     ", paste(utils::head(x$col_rel_width, -1L), collapse = ":"),
      "\n", sep = "")
  cat("  Record key: ", x$record_id, " (hidden at render time)\n", sep = "")
  if (nrow(x$data) > 0L) {
    nshow <- min(as.integer(n), nrow(x$data))
    cat(sprintf("\n  First %d row%s:\n", nshow, if (nshow == 1L) "" else "s"))
    out <- utils::capture.output(
      print(utils::head(x$data[, vis, drop = FALSE], nshow), row.names = FALSE))
    cat(paste0("  ", out), sep = "\n")
  }
  invisible(x)
}
