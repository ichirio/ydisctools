# Internal helpers shared by the listing builders.  Not exported.

`%||%` <- function(a, b) if (!is.null(a)) a else b

# Display width of each string, accounting for wide (CJK) glyphs.  Falls back to
# the character count when the width is undeterminable.
.listing_disp_width <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  w <- suppressWarnings(nchar(x, type = "width", allowNA = TRUE))
  bad <- is.na(w)
  if (any(bad)) w[bad] <- nchar(x[bad], type = "chars")
  as.integer(w)
}

# Escape a literal string for use inside a regular expression.
.listing_regex_escape <- function(x) {
  gsub("([][{}()^$.|*+?\\\\])", "\\\\\\1", x)
}

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

# Wrap one cell into pieces of at most `width` display columns.
#
# Break priority, mirroring the G-MEDAS listing convention:
#   1. after the separator (`sep`, kept at the end of the piece) -- a semantic
#      break, so "A/B/C" prefers to break between the components;
#   2. at word boundaries (after whitespace, a comma or a hyphen);
#   3. hard-split a single token that is still too long.
#
# `width = NULL` / `NA` / `<= 0` disables wrapping (the text is returned as-is).
.listing_wrap <- function(text, width, sep = "/") {
  text <- if (length(text) == 0L || is.na(text)) "" else as.character(text)
  if (!nzchar(text)) return("")
  if (is.null(width) || is.na(width) || width <= 0) return(text)

  # (1) break after the separator, keeping it at the end of each piece.
  parts <- if (!is.null(sep) && nzchar(sep)) {
    strsplit(text, paste0("(?<=", .listing_regex_escape(sep), ")"), perl = TRUE)[[1L]]
  } else {
    text
  }
  if (!length(parts)) parts <- text

  out <- character(0)
  for (p in parts) {
    p <- trimws(p)
    if (!nzchar(p)) next
    if (.listing_disp_width(p) <= width) {
      out <- c(out, p)
      next
    }

    # (2) word boundaries.
    words <- strsplit(p, "(?<=\\s|,|-)", perl = TRUE)[[1L]]
    if (!length(words)) words <- p
    cur <- ""
    for (w in words) {
      if (!nzchar(cur)) {
        cur <- w
      } else if (.listing_disp_width(paste0(cur, w)) <= width) {
        cur <- paste0(cur, w)
      } else {
        out <- c(out, trimws(cur))
        cur <- w
      }
      # (3) hard-split a token that is over-long on its own.
      while (.listing_disp_width(trimws(cur)) > width) {
        piece <- .listing_take(trimws(cur), width)
        out   <- c(out, piece)
        cur   <- substring(trimws(cur), nchar(piece, type = "chars") + 1L)
      }
    }
    if (nzchar(trimws(cur))) out <- c(out, trimws(cur))
  }

  if (!length(out)) out <- ""
  out
}

# Per-record line count: the tallest cell across all columns of that record.
# `cols` is a list of columns, each a list of character vectors (the wrapped
# pieces of one cell).
.listing_row_heights <- function(cols) {
  n <- length(cols[[1L]])
  vapply(seq_len(n), function(i) {
    max(vapply(cols, function(cl) length(cl[[i]]), integer(1L)))
  }, integer(1L))
}

# Pad every cell of a column with "" so it occupies exactly `heights[i]` lines.
.listing_pad <- function(col, heights) {
  lapply(seq_along(col), function(i) {
    v <- col[[i]]
    if (length(v) < heights[[i]]) c(v, rep("", heights[[i]] - length(v))) else v
  })
}

# Compose one display column from its source columns, joining with `sep` and
# skipping NA / empty values (the `catx()` convention).
.listing_compose <- function(data, spec) {
  vars <- spec$vars
  missing <- setdiff(vars, names(data))
  if (length(missing)) {
    stop("listing_col(): column(s) not found in `data`: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  pieces <- lapply(vars, function(v) {
    x <- data[[v]]
    x <- as.character(x)
    x[is.na(x)] <- ""
    x
  })
  n <- nrow(data)
  vapply(seq_len(n), function(i) {
    vals <- vapply(pieces, `[[`, character(1L), i)
    vals <- vals[nzchar(vals)]
    paste(vals, collapse = spec$sep)
  }, character(1L))
}
