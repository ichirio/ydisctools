# Internal helpers shared by the listing builders.  Not exported.
#
# rtfreporter keeps its own pipeline steps private (`.apply_stub_vars()`,
# `.stub_remap_styles()`, ...) and exports only the standalone utilities.  The
# split here follows that: `listing_wrap()` is exported because wrapping a
# string to a display width is a job of its own, while composing, measuring and
# padding are steps with no life outside `rtf_listing()`.

`%||%` <- function(a, b) if (!is.null(a)) a else b

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
    x <- as.character(data[[v]])
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

# One source column's display name: its `label` attribute when it has a usable
# one, otherwise the column name.  This is `stub_cols(label = NULL)`'s rule --
# the listing follows rtfreporter's convention rather than inventing its own.
.listing_label <- function(data, v) {
  lab <- attr(data[[v]], "label", exact = TRUE)
  if (is.character(lab) && length(lab) == 1L && !is.na(lab) && nzchar(lab)) {
    lab
  } else {
    v
  }
}

# Resolve a display column's header.
#
# An explicit `header =` is returned untouched -- the caller laid the lines out
# by hand and re-wrapping would fight them.  An auto-generated header is built
# from every source column (label, else name), joined with the column's `sep`
# and a newline, and wrapped to the column width so it cannot be wider than the
# column it sits over.
.listing_resolve_header <- function(data, spec, width) {
  if (!is.null(spec$header)) return(spec$header)
  labs <- vapply(spec$vars, function(v) .listing_label(data, v), character(1L))
  n    <- length(labs)
  pieces <- if (n > 1L) c(paste0(labs[-n], spec$sep), labs[[n]]) else labs
  lines <- unlist(lapply(pieces, .listing_wrap, width = width, sep = spec$sep),
                  use.names = FALSE)
  paste(lines, collapse = "\n")
}

# Per-record line count: the tallest cell across all columns of that record.
# `cols` is a list of columns, each a list of one entry per record.  An entry
# that is not a list/vector of lines counts as a single line (the tolerant
# handling proposed in #84).
.listing_line_counts <- function(cols) {
  n <- length(cols[[1L]])
  vapply(seq_len(n), function(i) {
    max(vapply(cols, function(cl) {
      e <- cl[[i]]
      if (is.list(e) || is.character(e)) length(e) else 1L
    }, integer(1L)))
  }, integer(1L))
}

# Pad every cell of a column so it occupies exactly `heights[i]` lines.
#
# `fill_down = TRUE` repeats a single-line value down the record instead of
# padding with "".  That is what lets rtfreporter's `collapse_repeats` see one
# constant value per record and suppress the run -- and, because it suppresses
# per page, reprint the value at the top of the next page.  A cell that already
# wraps onto several lines is blank-padded as usual: there is no single value
# to carry, and printing it in full is never wrong, only less tidy.
.listing_pad <- function(col, heights, fill_down = FALSE) {
  lapply(seq_along(col), function(i) {
    v <- col[[i]]
    if (!is.list(v) && !is.character(v)) v <- as.character(v)
    n <- heights[[i]]
    if (length(v) >= n) return(v)
    if (fill_down && length(v) == 1L) return(rep(v, n))
    c(v, rep("", n - length(v)))
  })
}

# Width of one character, in twips, for the listing's font.
#
# rtfreporter's internal length unit is twips, so a character-based width only
# becomes a column width by going through the font.  `text_width_in()` is
# monospace-exact for the default Courier New and an approximation elsewhere.
.listing_char_twips <- function(font, size_half_points) {
  if (!requireNamespace("rtfreporter", quietly = TRUE)) {
    stop("Absolute (twips) listing widths need the \"rtfreporter\" package. ",
         "Install it from https://github.com/ichirio/rtfreporter, or use the ",
         "relative `spacer_rel_width` instead.", call. = FALSE)
  }
  rtfreporter::text_width_in("0", font = font,
                             size_half_points = size_half_points) * 1440
}

# Scale a twips width vector so it sums to `total`, holding the columns marked
# `fixed` at their given width.
#
# The spacer columns are the fixed ones: scaling a 1-twip divider along with
# everything else would inflate it to something visible and quietly discard the
# width the caller asked for.  Rounding drift is absorbed by the widest
# scalable column so the total is exact.
.listing_scale_twips <- function(twips, fixed, total) {
  total <- as.integer(round(total))
  free  <- !fixed
  if (!any(free)) return(twips)
  budget <- total - sum(twips[fixed])
  if (budget <= 0L) {
    stop("`table_width_twips` (", total, ") leaves no room for the content ",
         "columns: the spacer columns alone need ", sum(twips[fixed]),
         " twips.", call. = FALSE)
  }
  twips[free] <- as.integer(round(twips[free] * (budget / sum(twips[free]))))
  twips[free] <- pmax(twips[free], 1L)
  drift <- total - sum(twips)
  if (drift != 0L) {
    j <- which(free)[which.max(twips[free])]
    twips[j] <- max(1L, twips[j] + drift)
  }
  twips
}
