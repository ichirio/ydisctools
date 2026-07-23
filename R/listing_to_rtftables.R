#' Hand a listing to rtfreporter
#'
#' Converts an [rtf_listing()] result into \pkg{rtfreporter} page objects by
#' calling `rtfreporter::as_rtftables()` with the listing's metadata and the
#' pagination settings a listing needs. This is the only point where
#' \pkg{ydisctools} talks to \pkg{rtfreporter}; the dependency runs one way, so
#' the listing builders themselves stay renderer-agnostic.
#'
#' @details
#' Pagination is delegated to `as_rtftables()` rather than reimplemented. The
#' listing's hidden record key is passed as `group_col` (and as `drop_cols`, so
#' it never prints), and `split = "group_safe"` then packs whole records onto a
#' page and spills on overflow -- a record is never cut across a page boundary.
#' `blank_rows = "between_groups"` puts a blank line between records and
#' `count_blank_rows = TRUE` charges those blanks to the page budget, so
#' `max_rows` means what it says.
#'
#' A record taller than `max_rows` is force-split with rtfreporter's `(Cont.)`
#' machinery. Note that because the group column is hidden, the `(Cont.)`
#' marker itself is not printed.
#'
#' @param x An `rtflisting` from [rtf_listing()].
#' @param max_rows Maximum body rows per page, or `NULL` (default) for a single
#'   un-split page.
#' @param blank_row_first,blank_row_end Add a blank row at the top / bottom of
#'   each page (both default `TRUE`, the usual listing furniture).
#' @param ... Further arguments passed to `rtfreporter::as_rtftables()`.
#'
#' @return A list of `rtftable` objects -- one per page -- ready for
#'   `rtfreporter::rtf_tables()`.
#'
#' @seealso [rtf_listing()]
#'
#' @examples
#' \dontrun{
#' lst   <- rtf_listing(adsl, listing_col(USUBJID, width = 15))
#' pages <- listing_to_rtftables(lst, max_rows = 40)
#'
#' rtfreporter::rtf_document() |>
#'   rtfreporter::rtf_section() |>
#'   rtfreporter::rtf_tables(pages)
#' }
#' @export
listing_to_rtftables <- function(x, max_rows = NULL,
                                 blank_row_first = TRUE,
                                 blank_row_end = TRUE, ...) {
  if (!inherits(x, "rtflisting")) {
    stop("`x` must be an rtflisting (see rtf_listing()).", call. = FALSE)
  }
  if (!requireNamespace("rtfreporter", quietly = TRUE)) {
    stop("Package \"rtfreporter\" is required to render a listing. ",
         "Install it from https://github.com/ichirio/rtfreporter.",
         call. = FALSE)
  }
  if (!is.null(max_rows)) {
    max_rows <- as.integer(max_rows)
    if (length(max_rows) != 1L || is.na(max_rows) || max_rows < 1L) {
      stop("`max_rows` must be a single positive integer, or NULL.",
           call. = FALSE)
    }
  }

  args <- list(
    x$data,
    col_header       = x$col_header,
    col_rel_width    = x$col_rel_width,
    col_spec         = x$col_spec,
    group_col        = x$record_id,
    group_by         = "value",
    drop_cols        = x$record_id,
    split            = if (is.null(max_rows)) "none" else "group_safe",
    blank_rows       = "between_groups",
    count_blank_rows = TRUE,
    blank_row_first  = blank_row_first,
    blank_row_end    = blank_row_end,
    read_meta        = FALSE
  )
  if (!is.null(max_rows)) args$max_rows <- max_rows

  # Caller overrides win.
  extra <- list(...)
  for (nm in names(extra)) args[[nm]] <- extra[[nm]]

  do.call(rtfreporter::as_rtftables, args)
}
