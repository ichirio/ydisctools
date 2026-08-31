#' Wrap a listing cell to a display width
#'
#' Breaks one cell of a clinical listing into the physical lines it occupies,
#' following the listing convention: break **after the separator** first, then
#' at word boundaries, and only then hard-split a token that is still too long.
#' This is the wrapping step [rtf_listing()] applies to every cell; it is
#' exported so you can measure, preview or reuse it on its own.
#'
#' `width` is a **display width in characters**, not a character count: a
#' full-width (CJK) glyph counts as two, so a Japanese listing wrapped to
#' `width = 20` really does fit in twenty monospaced columns. `nchar()` would
#' count those glyphs as one apiece and overflow the column.
#'
#' @section Break priority:
#' 1. **After `sep`.** `"Ovarian cancer/BRCA1 mutation"` prefers to break
#'    between its components, and the separator stays at the end of the line it
#'    closes -- the look a stacked listing column is expected to have.
#' 2. **At a word boundary** -- after whitespace, a comma or a hyphen.
#' 3. **Hard split**, for a single token wider than `width` on its own (a long
#'    identifier, a URL). Every returned line is then guaranteed to fit.
#'
#' @section Stacked or flowed:
#' `layout` decides what happens to a cell that would *fit* on one line:
#'
#' * `"stack"` (default) breaks after **every** separator regardless of length,
#'   so `"40/F"` becomes two lines. This is the conventional listing look: one
#'   source column per line, and a column reads down.
#' * `"flow"` treats the separator as a break *opportunity* and fills each line
#'   as far as `width` allows, so `"40/F"` stays on one line. Use it for
#'   columns whose parts are short -- an age and a sex, a value and its unit --
#'   where a stacked pair wastes two rows on four characters.
#'
#' @param text The cell text. A character vector is allowed; each element is
#'   wrapped independently.
#' @param width Maximum display width in characters. `NULL`, `NA` or a value
#'   `<= 0` means "no width limit": `"stack"` still breaks at every separator,
#'   `"flow"` returns the text unchanged.
#' @param sep The separator to break after (default `"/"`). `""` or `NULL`
#'   skips priority 1 and wraps on word boundaries only.
#' @param layout `"stack"` (default) or `"flow"` -- see *Stacked or flowed*.
#'
#' @return For a `text` of length 1, a character vector of lines. For a longer
#'   `text`, a list of such character vectors, one per element.
#'
#' @seealso [rtf_listing()], which applies this to every cell;
#'   [auto_listing_widths()] for choosing `width`;
#'   [split_column_by_max_bytes()] for the byte-budget (SAS transport) split,
#'   which solves a different problem.
#'
#' @examples
#' listing_wrap("Ovarian cancer/BRCA1 mutation/Serous adenocarcinoma", 22)
#'
#' # An over-long token is hard-split rather than overflowing the column.
#' listing_wrap("Immunohistochemistry", 8)
#'
#' # Vectorised: one element in, one character vector out.
#' listing_wrap(c("Partial response", "Progressive disease"), 10)
#'
#' # Short parts: stacked by default, side by side with layout = "flow".
#' listing_wrap("40/F", 20)
#' listing_wrap("40/F", 20, layout = "flow")
#'
#' # "flow" still breaks once the line is full.
#' listing_wrap("40/F/Screening/Completed", 12, layout = "flow")
#' @export
listing_wrap <- function(text, width, sep = "/",
                         layout = c("stack", "flow")) {
  layout <- match.arg(layout)
  if (!is.null(sep) && (!is.character(sep) || length(sep) != 1L || is.na(sep))) {
    stop("`sep` must be a single string, or NULL.", call. = FALSE)
  }
  if (length(text) == 1L) return(.listing_wrap(text, width, sep, layout))
  lapply(text, .listing_wrap, width = width, sep = sep, layout = layout)
}

# ---------------------------------------------------------------------------
# Internals.  Kept private for the same reason rtfreporter keeps its pipeline
# steps private: outside the pipeline they have no job to do.
# ---------------------------------------------------------------------------

# Display width of each string, accounting for wide (CJK) glyphs.  Falls back
# to the character count when the width is undeterminable.
.listing_disp_width <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  w <- suppressWarnings(nchar(x, type = "width", allowNA = TRUE))
  bad <- is.na(w)
  if (any(bad)) w[bad] <- nchar(x[bad], type = "chars")
  as.integer(w)
}

# PCRE literal quoting.  \Q...\E is preferred over escaping each metacharacter
# because this package's notes warn that some local R builds mishandle gsub()
# back-references -- and a lookbehind over quoted text is still fixed-width,
# which PCRE requires.
.listing_quote <- function(x) paste0("\\Q", x, "\\E")

# Longest prefix of `x` whose display width is <= `width`.  Always returns at
# least one character, so callers that loop on the remainder make progress even
# when a single glyph is wider than `width`.
.listing_take <- function(x, width) {
  n <- nchar(x, type = "chars")
  if (n == 0L) return("")
  best <- 1L
  for (i in seq_len(n)) {
    if (.listing_disp_width(substr(x, 1L, i)) <= width) best <- i else break
  }
  substr(x, 1L, best)
}

# Split after every separator, keeping the separator at the end of its piece.
.listing_split_sep <- function(text, sep) {
  if (is.null(sep) || !nzchar(sep)) return(text)
  parts <- strsplit(text, paste0("(?<=", .listing_quote(sep), ")"),
                    perl = TRUE)[[1L]]
  if (!length(parts)) text else parts
}

# Refill separator-delimited pieces into lines of at most `width`, so a cell
# whose parts are short keeps them side by side.  This is what turns the
# unconditional break of "stack" into the break *opportunity* of "flow".
.listing_flow <- function(parts, width) {
  out <- character(0)
  cur <- ""
  for (p in parts) {
    if (!nzchar(cur)) {
      cur <- p
    } else if (.listing_disp_width(trimws(paste0(cur, p))) <= width) {
      cur <- paste0(cur, p)
    } else {
      out <- c(out, cur)
      cur <- p
    }
  }
  if (nzchar(cur)) out <- c(out, cur)
  out
}

# Wrap one cell.  See listing_wrap() above for the break priority and layouts.
.listing_wrap <- function(text, width, sep = "/", layout = "stack") {
  text <- if (length(text) == 0L || is.na(text[[1L]])) {
    ""
  } else {
    as.character(text)[[1L]]
  }
  if (!nzchar(text)) return("")
  no_width <- is.null(width) || is.na(width) || width <= 0

  # (1) break after the separator.  "stack" keeps every break; "flow" refills
  # the pieces into lines, so a break only survives where the line ran out.
  parts <- .listing_split_sep(text, sep)
  if (identical(layout, "flow")) {
    parts <- if (no_width) paste(parts, collapse = "") else
      .listing_flow(parts, width)
  }
  if (no_width) return(trimws(parts))

  out <- character(0)
  for (p in parts) {
    p <- trimws(p)
    if (!nzchar(p)) next
    if (.listing_disp_width(p) <= width) {
      out <- c(out, p)
      next
    }

    # (2) word boundaries.  Each word keeps the separator it was split after,
    # so the space between two words survives into the joined line.
    words <- strsplit(p, "(?<=\\s|,|-)", perl = TRUE)[[1L]]
    if (!length(words)) words <- p
    cur <- ""
    for (w in words) {
      # (3) a token wider than the column on its own.  Split it here rather
      # than after accumulating: trimming the running line to measure it would
      # eat the trailing space that separates this word from the next.
      if (.listing_disp_width(trimws(w)) > width) {
        if (nzchar(trimws(cur))) out <- c(out, trimws(cur))
        tok <- sub("^\\s+", "", w)
        while (.listing_disp_width(trimws(tok)) > width) {
          piece <- .listing_take(tok, width)
          out   <- c(out, piece)
          tok   <- substring(tok, nchar(piece, type = "chars") + 1L)
        }
        cur <- tok                      # keeps any trailing separator
        next
      }
      if (!nzchar(cur)) {
        cur <- w
      } else if (.listing_disp_width(trimws(paste0(cur, w))) <= width) {
        cur <- paste0(cur, w)
      } else {
        out <- c(out, trimws(cur))
        cur <- w
      }
    }
    if (nzchar(trimws(cur))) out <- c(out, trimws(cur))
  }

  if (!length(out)) out <- ""
  out
}
