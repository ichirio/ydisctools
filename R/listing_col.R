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
#' @param name Output column name. `NULL` (default) derives one from the source
#'   columns.
#'
#' @return An `rtf_listing_col` object (a specification consumed by
#'   [rtf_listing()]).
#'
#' @seealso [rtf_listing()], [auto_listing_widths()]
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
#' @export
listing_col <- function(..., header = NULL, width = NULL, sep = "/",
                        name = NULL) {
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

  structure(
    list(
      vars   = vars,
      header = header,          # NULL = resolve from the data in rtf_listing()
      width  = width,
      sep    = sep,
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
  invisible(x)
}
