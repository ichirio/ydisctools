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
#' the renderer. Use [suggest_listing_widths()] to get sensible starting values
#' from the data, then tune them by eye.
#'
#' @param ... Source columns feeding this display column, given as bare names
#'   or strings, in the order they should be joined.
#' @param header Column header text, or `NULL` (default) to use the first
#'   source column's name. May contain `\n` for a multi-line header.
#' @param width Maximum display width in characters. Cells wider than this are
#'   wrapped. `NULL` (default) disables wrapping for this column; its relative
#'   width is then measured from the data.
#' @param sep Separator used to join the source columns (default `"/"`). It is
#'   also the preferred wrapping point -- a cell breaks after a separator
#'   before it breaks mid-phrase.
#' @param name Output column name. `NULL` (default) derives one from the source
#'   columns.
#'
#' @return A `listing_col` object (a specification consumed by
#'   [rtf_listing()]).
#'
#' @seealso [rtf_listing()], [suggest_listing_widths()]
#'
#' @examples
#' listing_col(USUBJID, width = 15)
#' listing_col(DISPTPD, BRCA, HIST,
#'             header = "Primary Diagnosis/\nAny (BRCA) Mutations/\nHistology",
#'             width  = 22)
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
      header = header %||% vars[[1L]],
      width  = width,
      sep    = sep,
      name   = name %||% paste(vars, collapse = "_")
    ),
    class = "listing_col"
  )
}

#' @export
print.listing_col <- function(x, ...) {
  cat(sprintf("<listing_col> %s\n", x$name))
  cat("  Source:  ", paste(x$vars, collapse = paste0(" ", x$sep, " ")), "\n", sep = "")
  cat("  Header:  ", gsub("\n", " / ", x$header), "\n", sep = "")
  cat("  Width:   ", if (is.null(x$width)) "unwrapped" else x$width, "\n", sep = "")
  invisible(x)
}

