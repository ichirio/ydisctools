#' Suggest starting column widths for a listing
#'
#' Picking the per-column `width` values for [listing_col()] by hand is
#' tedious: too narrow and cells wrap into a tall, ragged record, too wide and
#' the listing overflows the page. `suggest_listing_widths()` proposes a
#' starting set from the data, which you then tune by eye against the rendered
#' output.
#'
#' The proposal is content-driven: each column is composed exactly as
#' [rtf_listing()] would compose it, its cell widths are measured (display
#' width, so wide CJK glyphs count as two), and the `probs` quantile of that
#' distribution is taken as the column's natural demand -- a quantile rather
#' than the maximum, so one unusually long value does not dominate the layout.
#' The header's own longest line is respected as a floor, and the demands are
#' then scaled to fit `total_width` and rounded, subject to `min_width`.
#'
#' @param data A data frame, as passed to [rtf_listing()].
#' @param ... [listing_col()] specifications, in display order.
#' @param total_width Total display width available for the content columns, in
#'   characters (default `150`, a landscape A4 page in a small monospaced
#'   font). Spacer columns are not counted.
#' @param min_width Minimum width any column may be given (default `6`).
#' @param probs Quantile of each column's cell widths taken as its natural
#'   demand (default `0.9`).
#'
#' @return A named integer vector of suggested widths, one per column, in the
#'   order the specifications were given. Printing it shows a pasteable
#'   summary next to each column's measured demand.
#'
#' @seealso [listing_col()], [rtf_listing()]
#'
#' @examples
#' adsl <- data.frame(
#'   USUBJID = c("63016-204", "63016-205"),
#'   DISPTPD = c("Ovarian cancer", "Fallopian tube cancer"),
#'   STAGE   = c("IIIC", "IV"),
#'   stringsAsFactors = FALSE
#' )
#' suggest_listing_widths(
#'   adsl,
#'   listing_col(USUBJID, header = "Unique\nSubject ID"),
#'   listing_col(DISPTPD, header = "Primary Diagnosis"),
#'   listing_col(STAGE,   header = "Stage"),
#'   total_width = 60
#' )
#' @export
suggest_listing_widths <- function(data, ..., total_width = 150,
                                   min_width = 6, probs = 0.9) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  specs <- list(...)
  if (!length(specs) || !all(vapply(specs, inherits, logical(1L), "listing_col"))) {
    stop("`...` must be one or more listing_col() specifications.", call. = FALSE)
  }
  total_width <- as.numeric(total_width)
  min_width   <- as.integer(min_width)
  if (length(total_width) != 1L || is.na(total_width) || total_width <= 0) {
    stop("`total_width` must be a single positive number.", call. = FALSE)
  }
  if (length(min_width) != 1L || is.na(min_width) || min_width < 1L) {
    stop("`min_width` must be a single positive integer.", call. = FALSE)
  }
  if (length(probs) != 1L || is.na(probs) || probs < 0 || probs > 1) {
    stop("`probs` must be a single number in [0, 1].", call. = FALSE)
  }

  nm <- vapply(specs, function(s) s$name, character(1L))

  # Natural demand per column: the `probs` quantile of its cell widths, floored
  # by the widest single line of its header.
  demand <- vapply(seq_along(specs), function(j) {
    vals <- if (nrow(data)) .listing_compose(data, specs[[j]]) else character(0)
    cell <- if (length(vals)) {
      as.numeric(stats::quantile(.listing_disp_width(vals), probs = probs,
                                 names = FALSE, type = 7))
    } else 0
    head_w <- max(c(0L, .listing_disp_width(
      strsplit(specs[[j]]$header, "\n", fixed = TRUE)[[1L]])))
    max(cell, head_w, min_width)
  }, numeric(1L))

  # Scale to the available width, then round while respecting `min_width`.
  scaled <- demand * (total_width / sum(demand))
  out    <- pmax(min_width, as.integer(round(scaled)))

  structure(stats::setNames(out, nm), class = c("listing_widths", "integer"),
            demand = stats::setNames(round(demand, 1L), nm),
            total_width = total_width)
}

#' @export
print.listing_widths <- function(x, ...) {
  dem <- attr(x, "demand", exact = TRUE)
  cat(sprintf("<listing_widths> %d columns, total %d of %g\n",
              length(x), sum(unclass(x)), attr(x, "total_width", exact = TRUE)))
  w <- max(nchar(names(x)))
  for (i in seq_along(x)) {
    cat(sprintf("  %-*s  width = %3d   (measured demand %.1f)\n",
                w, names(x)[i], unclass(x)[i], dem[i]))
  }
  cat("\nPaste into your listing_col() calls, then tune by eye.\n")
  invisible(x)
}
