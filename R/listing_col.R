#' Describe one column of a clinical listing
#'
#' `listing_col()` declares a single **display column** of a listing built by
#' [rtf_listing()]: which source column(s) feed it, the header it carries, and
#' the width its cells are wrapped to.
#'
#' Several source columns are joined with `sep` -- skipping `NA` and empty
#' values, the [catx()] convention -- so
#' `listing_col(DISPTPD, BRCA, HIST)` produces `"<DISPTPD>/<BRCA>/<HIST>"` with
#' missing components dropped rather than leaving empty separators behind.
#'
#' `width` is a **display width in characters** (wide CJK glyphs count as two),
#' and it does double duty: cells longer than it are wrapped onto extra lines,
#' and the widths of all columns become the relative column widths handed to
#' the renderer. Use [auto_listing_widths()] to get sensible starting values
#' from the data, then tune them by eye.
#'
#' @section Automatic headers:
#' With `header = NULL` (the default) the header is built from **every** source
#' column, not just the first: each column's `label` attribute when it has one,
#' otherwise its name, joined with the column's `sep` and a newline. So
#' `listing_col(DISPTPD, BRCA, HIST)` over labelled data yields
#' `"Primary Diagnosis/\nBRCA/\nHistology"`.
#'
#' This is deliberately the same rule `rtfreporter::stub_cols(label = NULL)`
#' uses for a merged stub column, so a listing header and a table stub label
#' are derived the same way.
#'
#' An automatic header is wrapped to `width`, and so can never be wider than
#' the column it sits over. A `header` you write yourself is used **exactly as
#' written** -- you laid the lines out, and re-wrapping would fight you.
#'
#' Because the labels live on the data, the header is resolved by
#' [rtf_listing()] (which has the data) rather than here.
#'
#' @section Stacked or flowed:
#' `layout` decides what a cell does when its parts would fit side by side:
#'
#' * `"stack"` (default) breaks after every separator regardless of length, so
#'   `listing_col(AGE, SEX)` prints `40` and `F` on two lines. One source
#'   column per line is the conventional listing look, and it keeps a column
#'   readable down the page.
#' * `"flow"` fills each line as far as `width` allows, so the same column
#'   prints `40/F` on one line and only breaks when it runs out of room. Use it
#'   where a stacked pair would waste two rows on four characters.
#'
#' The choice is per column, because a listing usually wants both: a long
#' diagnosis stacked, an age and a sex flowed.
#'
#' @section Repeat suppression:
#' In a listing sorted by subject and visit, the subject belongs on the first
#' row of its run and nowhere else. `collapse_repeats = TRUE` marks this column
#' for that treatment: its value is carried down every physical row of a record
#' and then blanked by `rtfreporter::as_rtftables(collapse_repeats = )`, which
#' keeps only the first row of each run.
#'
#' Delegating rather than blanking the value here is what makes the value
#' **reappear at the top of every page**: rtfreporter suppresses per page,
#' after the split, so a run continued across a page break still shows its
#' subject at the top of the new page. Blanking at composition time could not
#' know where the page breaks fall.
#'
#' Suppression is hierarchical in the order the columns are declared, so
#' marking the subject column and then the visit column restarts the visit run
#' whenever the subject changes.
#'
#' A cell that wraps onto more than one line is left alone -- it is printed in
#' full rather than suppressed, which is never wrong, only less tidy. Repeat
#' suppression is for short key columns; widen the column if you want it.
#'
#' @param ... Source columns feeding this display column, given as bare names
#'   or strings, in the order they should be joined.
#' @param header Column header text, or `NULL` (default) to build one from the
#'   source columns -- see *Automatic headers*. May contain `\n` for a
#'   multi-line header.
#' @param width Maximum display width in characters. Cells wider than this are
#'   wrapped. `NULL` (default) leaves the width to [rtf_listing()]: it is
#'   fitted to that call's `total_width` when one is given, and otherwise
#'   measured from the data with no wrapping.
#' @param sep Separator used to join the source columns (default `"/"`). It is
#'   also the preferred wrapping point -- a cell breaks after a separator
#'   before it breaks mid-phrase -- and the separator an automatic header
#'   carries between its parts.
#' @param layout `"stack"` (default) or `"flow"` -- see *Stacked or flowed*.
#' @param collapse_repeats Blank this column on every row but the first of each
#'   run of equal values (default `FALSE`) -- see *Repeat suppression*.
#' @param name Output column name. `NULL` (default) derives one from the source
#'   columns.
#'
#' @return An `rtf_listing_col` object (a specification consumed by
#'   [rtf_listing()]).
#'
#' @seealso [rtf_listing()], [auto_listing_widths()], [listing_wrap()]
#'
#' @examples
#' listing_col(USUBJID, width = 15)
#'
#' # Three source columns in one display column, header written by hand.
#' listing_col(DISPTPD, BRCA, HIST,
#'             header = "Primary Diagnosis/\nAny (BRCA) Mutations/\nHistology",
#'             width  = 22)
#'
#' # The same column with the header left to rtf_listing().
#' listing_col(DISPTPD, BRCA, HIST, width = 22)
#'
#' # Short parts side by side rather than stacked: "40/F", not "40" over "F".
#' listing_col(AGE, SEX, width = 8, layout = "flow")
#'
#' # A sort key that prints once per run.
#' listing_col(USUBJID, width = 12, collapse_repeats = TRUE)
#' @export
listing_col <- function(..., header = NULL, width = NULL, sep = "/",
                        layout = c("stack", "flow"),
                        collapse_repeats = FALSE,
                        name = NULL) {
  layout <- match.arg(layout)
  # Accept both bare symbols (`USUBJID`) and strings (`"USUBJID"`); anything
  # else is evaluated in the caller's frame.
  exprs <- as.list(substitute(list(...)))[-1L]
  env   <- parent.frame()
  vars  <- vapply(exprs, function(e) {
    if (is.character(e)) e[[1L]]
    else if (is.symbol(e)) as.character(e)
    else as.character(eval(e, env))[[1L]]
  }, character(1L))
  if (!length(vars)) {
    stop("listing_col() needs at least one source column.", call. = FALSE)
  }
  if (!is.null(width)) {
    width <- as.integer(width)
    if (length(width) != 1L || is.na(width) || width < 1L) {
      stop("`width` must be a single positive integer, or NULL.", call. = FALSE)
    }
  }
  if (!is.character(sep) || length(sep) != 1L || is.na(sep)) {
    stop("`sep` must be a single string.", call. = FALSE)
  }
  if (!is.null(header) &&
      (!is.character(header) || length(header) != 1L || is.na(header))) {
    stop("`header` must be NULL or a single string.", call. = FALSE)
  }
  if (!is.null(name) &&
      (!is.character(name) || length(name) != 1L || is.na(name))) {
    stop("`name` must be NULL or a single string.", call. = FALSE)
  }
  if (!is.logical(collapse_repeats) || length(collapse_repeats) != 1L ||
      is.na(collapse_repeats)) {
    stop("`collapse_repeats` must be TRUE or FALSE.", call. = FALSE)
  }

  structure(
    list(
      vars   = vars,
      header = header,          # NULL = resolve from the data in rtf_listing()
      width  = width,
      sep    = sep,
      layout = layout,
      collapse_repeats = collapse_repeats,
      name   = name %||% paste(vars, collapse = "_")
    ),
    class = "rtf_listing_col"
  )
}

#' @export
print.rtf_listing_col <- function(x, ...) {
  cat(sprintf("<rtf_listing_col> %s\n", x$name))
  cat("  Source:  ", paste(x$vars, collapse = paste0(" ", x$sep, " ")), "\n",
      sep = "")
  cat("  Header:  ",
      if (is.null(x$header)) {
        "<auto: labels of the source columns>"
      } else {
        gsub("\n", " / ", x$header)
      }, "\n", sep = "")
  cat("  Width:   ",
      if (is.null(x$width)) "<auto>" else x$width, "\n", sep = "")
  cat("  Layout:  ", x$layout,
      if (identical(x$layout, "stack")) " (one line per source column)"
      else " (side by side until the line is full)", "\n", sep = "")
  if (isTRUE(x$collapse_repeats)) {
    cat("  Repeats: suppressed (first row of each run only)\n")
  }
  invisible(x)
}
