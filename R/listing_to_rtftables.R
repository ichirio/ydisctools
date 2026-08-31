#' Hand a listing to rtfreporter
#'
#' Renders an [rtf_listing()] result into \pkg{rtfreporter} page objects by
#' calling `rtfreporter::as_rtftables()` with the layout recorded in the
#' listing's `"rtf_listing"` attribute and the pagination settings a listing
#' needs. This is the only point where \pkg{ydisctools} talks to
#' \pkg{rtfreporter}; the dependency runs one way, so [rtf_listing()] itself
#' stays renderer-agnostic and needs nothing installed.
#'
#' @details
#' Nothing here is listing-specific machinery -- every setting is an
#' `as_rtftables()` argument that already exists:
#'
#' * `row_title` = every column, i.e. left-align the lot;
#' * `group_col` / `drop_cols` = the hidden record key, so `split =
#'   "group_safe"` packs whole records onto a page and spills on overflow. A
#'   record is never cut across a page boundary;
#' * `blank_rows = "between_groups"` puts a blank line between records and
#'   `count_blank_rows = TRUE` charges those blanks to the page budget, so
#'   `max_rows` means what it says;
#' * `col_rel_width` or `column_widths_twips`, whichever [rtf_listing()]
#'   produced;
#' * `collapse_repeats` for the columns marked
#'   `listing_col(collapse_repeats = TRUE)`. Delegating the suppression is why
#'   a suppressed value **reappears at the top of every page**: rtfreporter
#'   suppresses per page, after the split.
#'
#' A record taller than `max_rows` is force-split with rtfreporter's `(Cont.)`
#' machinery. Note that because the group column is hidden, the `(Cont.)`
#' marker itself is not printed.
#'
#' @section Why this function exists:
#' Only because `as_rtftables()` does not yet read an `"rtf_listing"`
#' attribute. If the listing moves into rtfreporter, this collapses into
#' `as_rtftables(x, listing = ...)` -- the shape `stub = stub_spec(...)`
#' already has -- and the bridge disappears rather than being ported.
#'
#' @param x A `data.frame` from [rtf_listing()], with its `"rtf_listing"`
#'   attribute intact. Subsetting a data frame drops attributes, so make any
#'   edits before this call.
#' @param max_rows Maximum body rows per page, or `NULL` (default) for a single
#'   un-split page.
#' @param blank_row_first,blank_row_end Add a blank row at the top / bottom of
#'   each page (both default `TRUE`, the usual listing furniture).
#' @param ... Further arguments passed to `rtfreporter::as_rtftables()`. These
#'   win over the defaults above, so anything the listing does not decide for
#'   you -- borders, fonts, `paginate_cols()` -- is reachable.
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
#' print(pages[[1]])          # rtfreporter's own console preview
#'
#' rtfreporter::rtf_document() |>
#'   rtfreporter::rtf_tables(pages) |>
#'   rtfreporter::generate_rtfreport("listing.rtf", overwrite = TRUE)
#' }
#' @export
listing_to_rtftables <- function(x, max_rows = NULL,
                                 blank_row_first = TRUE,
                                 blank_row_end = TRUE, ...) {
  meta <- attr(x, "rtf_listing", exact = TRUE)
  if (!is.data.frame(x) || is.null(meta)) {
    stop("`x` must be a data.frame from rtf_listing(), with its ",
         "\"rtf_listing\" attribute intact (subsetting drops it).",
         call. = FALSE)
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
    x,
    col_header       = meta$col_header,
    row_title        = meta$row_title,
    group_col        = meta$record_id,
    group_by         = "value",
    drop_cols        = meta$record_id,
    split            = if (is.null(max_rows)) "none" else "group_safe",
    blank_rows       = "between_groups",
    count_blank_rows = TRUE,
    blank_row_first  = blank_row_first,
    blank_row_end    = blank_row_end,
    read_meta        = FALSE
  )
  # Relative or absolute, never both -- rtf_listing() settles which.
  if (!is.null(meta$column_widths_twips)) {
    args$column_widths_twips <- meta$column_widths_twips
  } else {
    args$col_rel_width <- meta$col_rel_width
  }
  if (!is.null(max_rows)) args$max_rows <- max_rows
  if (!is.null(meta$collapse_repeats)) {
    args$collapse_repeats <- meta$collapse_repeats
  }

  # With the header suppressed, the "tfl" preset's two rules -- header top and
  # header bottom -- collapse onto each other with nothing between them.  Move
  # the rule to the top of the body instead, so the table still has a top edge.
  if (is.list(meta$col_header) && !length(meta$col_header)) {
    s <- rtfreporter::rtf_border_side("single", 15L)
    args$border <- rtfreporter::rtf_table_border(
      header    = NULL,
      spanning  = NULL,
      body      = NULL,
      first_row = rtfreporter::rtf_border(top = s),
      last_row  = NULL
    )
  }

  # Caller overrides win.
  extra <- list(...)
  for (nm in names(extra)) args[[nm]] <- extra[[nm]]

  do.call(rtfreporter::as_rtftables, args)
}
