#' Shape a data frame into a clinical listing
#'
#' `rtf_listing()` turns a tidy source data frame into the shape a clinical
#' **listing** needs: one logical record may occupy several physical rows,
#' every cell of a record is wrapped to its column's width and padded so the
#' record's rows line up, narrow spacer columns separate the content columns,
#' and a hidden record key marks the record boundaries so the renderer can
#' paginate without ever splitting a record across pages.
#'
#' The result is a plain **`data.frame`** you can inspect, edit or write out,
#' with the layout it implies carried in an `"rtf_listing"` attribute.
#' [listing_to_rtftables()] reads that attribute and renders it; this function
#' itself only prepares data and needs no renderer.
#'
#' @section A listing is a table, not a new kind of object:
#' Everything a listing needs is something `rtfreporter::as_rtftables()`
#' already accepts, so no listing-specific page object exists:
#'
#' * **no row headings.** `row_title` selects the row-heading columns and its
#'   only effect is the default data alignment. A listing sets *every* column
#'   as a row heading, which is exactly "left-align everything";
#'   `row_title = integer(0)` would mean the opposite, no row heading at all.
#' * **records that never split.** The hidden record key is passed as
#'   `group_col` and `drop_cols`, and `split = "group_safe"` packs whole
#'   records onto a page.
#' * **blank lines between records.** `blank_rows = "between_groups"` with
#'   `count_blank_rows = TRUE`, so they are charged to the page budget.
#'
#' This mirrors `rtfreporter::stub_cols()`, which likewise returns a
#' `data.frame` and leaves the extra information in an `rtf_*` attribute.
#'
#' @section The `"type1"` layout:
#' The one layout implemented today reproduces the conventional G-MEDAS
#' listing style:
#'
#' 1. each display column is composed from its source columns, joined with the
#'    column's `sep` and skipping `NA` / empty values;
#' 2. each cell is wrapped to the column's `width` -- breaking after the
#'    separator first, then at word boundaries, then hard-splitting an
#'    over-long token (see [listing_wrap()]);
#' 3. every cell of a record is padded to the record's tallest cell, so the
#'    record's lines stay aligned across columns;
#' 4. a narrow spacer column is inserted between neighbouring content columns;
#' 5. all cells are left-aligned.
#'
#' Blank separator rows between records are **not** baked in here -- they are
#' added at render time by [listing_to_rtftables()], which lets the paginator
#' count them against the page budget.
#'
#' @section Column widths, relative or absolute:
#' By default the column widths are **relative**: a column's `width` in
#' characters is also its share of the table, and the spacers take
#' `spacer_rel_width` each. This is `rtfreporter::rtftable(col_rel_width = )`.
#'
#' Give `spacer_twips` instead and the whole width vector becomes **absolute**
#' (`column_widths_twips`), with the character widths converted through
#' `rtfreporter::text_width_in()` at `font` / `size_half_points`. That is what
#' a literal one-twip divider needs -- a relative width of 1 among content
#' widths of 12 to 24 is about a character wide, not a twip.
#'
#' When `table_width_twips` scales the table to the page, the **spacers are
#' held fixed** and only the content columns absorb the scaling. Scaling them
#' too would inflate a 1-twip divider into a visible gap and quietly discard
#' the width you asked for.
#'
#' @param data A data frame (or tibble): one row per logical record.
#' @param ... [listing_col()] specifications, in display order.
#' @param type Listing layout. Currently only `"type1"` (`1` is accepted as a
#'   shorthand).
#' @param header Whether to emit a column-header row. `TRUE` (default) uses
#'   each column's header; `FALSE` renders the listing with no header at all,
#'   for a continuation or sub-listing that borrows the header above it.
#' @param total_width Total display width, in characters, to fit the columns
#'   into. `NULL` (default) leaves every unset `width` to be measured from the
#'   data. When given, columns with an explicit `width` keep it and the
#'   remaining budget is shared out among the rest.
#' @param spacer_rel_width Relative width of the spacer columns inserted
#'   between content columns (default `1`). `0` inserts no spacer columns at
#'   all.
#' @param spacer_twips Absolute width of the spacer columns, in twips.
#'   `NULL` (default) uses `spacer_rel_width`. Setting it switches the whole
#'   table to absolute widths.
#' @param table_width_twips Total table width in twips, used only with
#'   `spacer_twips`. `NULL` (default) leaves the content columns at their
#'   natural width.
#' @param font,size_half_points Font and size used to convert character widths
#'   to twips, matching `rtfreporter::auto_col_widths()`. Only consulted with
#'   `spacer_twips`.
#' @param record_id Name of the hidden record-key column added to the output
#'   (default `".record_id"`). It is used for grouping / pagination and is
#'   dropped before rendering.
#'
#' @return A `data.frame` of physical listing rows, carrying an
#'   `"rtf_listing"` attribute: a list with `type`, `col_header`,
#'   `col_rel_width` or `column_widths_twips`, `row_title`, `record_id` and
#'   `n_records`. Subsetting the frame drops that attribute, so make any edits
#'   before handing it to [listing_to_rtftables()].
#'
#' @seealso [listing_col()] to describe a column, [auto_listing_widths()] for
#'   starting widths, [listing_wrap()] for the wrapping rule alone,
#'   [listing_to_rtftables()] to render.
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
#' head(lst)
#' attr(lst, "rtf_listing")$col_header
#'
#' # Headers built from the data: labels when present, names otherwise.
#' attr(adsl$DISPTPD, "label") <- "Primary Diagnosis"
#' attr(adsl$BRCA,    "label") <- "BRCA Mutation"
#' auto <- rtf_listing(adsl, listing_col(USUBJID, width = 12),
#'                     listing_col(DISPTPD, BRCA, width = 20))
#' attr(auto, "rtf_listing")$col_header
#'
#' # A literal one-twip divider between the content columns.
#' \dontrun{
#' rtf_listing(adsl,
#'             listing_col(USUBJID, width = 12),
#'             listing_col(DISPTPD, BRCA, width = 20),
#'             spacer_twips = 1, table_width_twips = 12960)
#' }
#' @export
rtf_listing <- function(data, ...,
                        type              = "type1",
                        header            = TRUE,
                        total_width       = NULL,
                        spacer_rel_width  = 1,
                        spacer_twips      = NULL,
                        table_width_twips = NULL,
                        font              = "courier_new",
                        size_half_points  = 18L,
                        record_id         = ".record_id") {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  specs <- list(...)
  if (!length(specs) ||
      !all(vapply(specs, inherits, logical(1L), "rtf_listing_col"))) {
    stop("`...` must be one or more listing_col() specifications.",
         call. = FALSE)
  }

  # `type1` / `1` both name the one layout there is.
  if (is.numeric(type) && length(type) == 1L && !is.na(type)) {
    type <- paste0("type", as.integer(type))
  }
  if (!identical(type, "type1")) {
    stop("`type` must be \"type1\" (the only layout implemented).",
         call. = FALSE)
  }
  if (!is.logical(header) || length(header) != 1L || is.na(header)) {
    stop("`header` must be TRUE or FALSE.", call. = FALSE)
  }
  spacer_rel_width <- as.numeric(spacer_rel_width)
  if (length(spacer_rel_width) != 1L || is.na(spacer_rel_width) ||
      spacer_rel_width < 0) {
    stop("`spacer_rel_width` must be a single non-negative number ",
         "(0 inserts no spacer columns).", call. = FALSE)
  }
  if (!is.null(spacer_twips)) {
    spacer_twips <- as.integer(round(spacer_twips))
    if (length(spacer_twips) != 1L || is.na(spacer_twips) ||
        spacer_twips < 1L) {
      stop("`spacer_twips` must be a single positive integer, or NULL. ",
           "Use `spacer_rel_width = 0` to drop the spacer columns.",
           call. = FALSE)
    }
  }
  if (!is.null(table_width_twips)) {
    if (is.null(spacer_twips)) {
      stop("`table_width_twips` scales absolute widths, so it needs ",
           "`spacer_twips` too; relative widths are scaled by the renderer.",
           call. = FALSE)
    }
    table_width_twips <- as.integer(round(table_width_twips))
    if (length(table_width_twips) != 1L || is.na(table_width_twips) ||
        table_width_twips < 1L) {
      stop("`table_width_twips` must be a single positive integer, or NULL.",
           call. = FALSE)
    }
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
  spacer_on <- (spacer_rel_width > 0 || !is.null(spacer_twips)) &&
    length(specs) >= 2L

  # 1. Compose each display column from its source columns.
  composed <- lapply(specs, function(s) .listing_compose(data, s))

  # 2. Settle each column's width, then its header (which is wrapped to it).
  widths  <- .listing_widths(data, specs, composed, total_width)
  headers <- vapply(seq_along(specs), function(j) {
    .listing_resolve_header(data, specs[[j]], widths[[j]])
  }, character(1L))

  if (nrow(data) == 0L) {
    out <- .listing_empty_frame(names_out, record_id, spacer_on)
  } else {
    # 3. Wrap every cell, using the column's own width only where the caller
    #    asked for one -- a measured width must not silently start wrapping.
    pieces <- lapply(seq_along(specs), function(j) {
      lapply(composed[[j]], .listing_wrap,
             width = specs[[j]]$width, sep = specs[[j]]$sep)
    })

    # 4. Align each record's cells to the record's tallest cell, then expand.
    heights <- .listing_line_counts(pieces)
    pieces  <- lapply(pieces, .listing_pad, heights = heights)

    out <- stats::setNames(
      lapply(pieces, function(col) unlist(col, use.names = FALSE)),
      names_out)
    out <- as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
    out[[record_id]] <- rep(seq_len(nrow(data)), times = heights)
    out <- .listing_insert_spacers(out, names_out, record_id, spacer_on)
  }

  .listing_attach_meta(
    out, names_out, headers, widths, spacer_on,
    type = type, header = header, record_id = record_id,
    spacer_rel_width = spacer_rel_width, spacer_twips = spacer_twips,
    table_width_twips = table_width_twips,
    font = font, size_half_points = size_half_points,
    n_records = nrow(data))
}

# Settle the width of every display column, in characters.
#
# An explicit `listing_col(width = )` always wins.  With `total_width` the
# columns that have none share what the explicit ones left; without it they are
# measured from their own content (and, having no declared width, are not
# wrapped).
.listing_widths <- function(data, specs, composed, total_width) {
  measured <- vapply(seq_along(specs), function(j) {
    hdr <- .listing_resolve_header(data, specs[[j]], NULL)
    max(c(1L, .listing_disp_width(composed[[j]]),
          .listing_disp_width(strsplit(hdr, "\n", fixed = TRUE)[[1L]])))
  }, numeric(1L))

  given <- vapply(specs, function(s) as.numeric(s$width %||% NA_real_),
                  numeric(1L))
  if (is.null(total_width)) {
    given[is.na(given)] <- measured[is.na(given)]
    return(given)
  }

  total_width <- as.numeric(total_width)
  if (length(total_width) != 1L || is.na(total_width) || total_width <= 0) {
    stop("`total_width` must be a single positive number, or NULL.",
         call. = FALSE)
  }
  free <- is.na(given)
  if (!any(free)) return(given)
  budget <- total_width - sum(given[!free])
  if (budget <= 0) {
    stop("`total_width` (", total_width, ") is already used up by the ",
         "columns with an explicit `width`.", call. = FALSE)
  }
  given[free] <- pmax(1, round(measured[free] * (budget / sum(measured[free]))))
  given
}

# Interleave narrow spacer columns between the content columns.  The hidden
# record-key column is kept last so it can be dropped at render time.
.listing_insert_spacers <- function(df, content, record_id, spacer_on) {
  if (!spacer_on) {
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

.listing_empty_frame <- function(content, record_id, spacer_on) {
  df <- as.data.frame(
    stats::setNames(rep(list(character(0)), length(content)), content),
    stringsAsFactors = FALSE, check.names = FALSE)
  df[[record_id]] <- integer(0)
  .listing_insert_spacers(df, content, record_id, spacer_on)
}

# Attach the layout metadata, in the *input* column coordinates -- i.e.
# including the hidden record-key column, which as_rtftables() reindexes away
# when it drops that column.
.listing_attach_meta <- function(out, content, headers, widths, spacer_on,
                                 type, header, record_id,
                                 spacer_rel_width, spacer_twips,
                                 table_width_twips, font, size_half_points,
                                 n_records) {
  # Interleave the spacers into the positional vectors.
  hdr <- character(0)
  rel <- numeric(0)
  spc <- logical(0)
  for (j in seq_along(content)) {
    if (j > 1L && spacer_on) {
      hdr <- c(hdr, "")
      rel <- c(rel, spacer_rel_width)
      spc <- c(spc, TRUE)
    }
    hdr <- c(hdr, headers[[j]])
    rel <- c(rel, widths[[j]])
    spc <- c(spc, FALSE)
  }
  # Absolute widths are settled over the *visible* columns only: the hidden
  # record-key placeholder is dropped before rendering, so counting it would
  # leave the rendered table one twip narrower than `table_width_twips`.
  twips <- NULL
  if (!is.null(spacer_twips)) {
    char_tw <- .listing_char_twips(font, size_half_points)
    twips <- pmax(as.integer(round(rel * char_tw)), 1L)
    twips[spc] <- spacer_twips
    if (!is.null(table_width_twips)) {
      twips <- .listing_scale_twips(twips, spc, table_width_twips)
    }
    rel <- NULL
  }

  # Placeholder entries for the hidden record-key column (last); as_rtftables()
  # drops them along with the column.
  hdr <- c(hdr, "")
  if (!is.null(rel))   rel   <- c(rel, 1)
  if (!is.null(twips)) twips <- c(twips, 1L)

  attr(out, "rtf_listing") <- list(
    type          = type,
    # Every column of a listing is a row heading, which is rtfreporter's way of
    # saying "left-align it".  Names, not positions, so dropping the record key
    # cannot shift them.
    row_title     = names(out),
    # `list()` -- an empty set of header rows -- is what suppresses the header
    # in rtfreporter.  `NULL` and `character(0)` both fall back to names(data).
    col_header    = if (header) hdr else list(),
    col_rel_width = rel,
    column_widths_twips = twips,
    record_id     = record_id,
    n_records     = as.integer(n_records)
  )
  out
}
