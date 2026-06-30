#' Collapse long-format statistic rows into one formatted value per group
#'
#' @description
#' `format_stats()` takes a long ("tidy") data frame in which each row carries a
#' single statistic (a `stat` name and a numeric `value`) and collapses the
#' several statistic rows that belong to the same display row into a single
#' formatted string. Which statistics are combined, the literal text around them
#' (parentheses, commas, ...) and the number of decimal places are all driven by
#' a **format-spec table** with inline digit tokens, so the helper is agnostic to
#' the names and the number of grouping-key columns.
#'
#' @details
#' The grouping keys that identify one output row are *every* column of `data`
#' except `stat_col` and `value_col`. Within each group the statistics are
#' rendered into `out_col` according to a template string looked up from `specs`.
#'
#' ## Format-spec table
#' `specs` is a data frame holding one `template_col` column plus one or more
#' **key columns**. The key columns must be a subset of the grouping keys of
#' `data` (for example `group2` and `label`); `specs` is left-joined onto `data`
#' by those key columns, so you can make the template as coarse (per `label`) or
#' as fine (per variable *and* `label`) as you need.
#'
#' ## Template tokens
#' A template is plain text with `{...}` tokens that are replaced by statistic
#' values:
#' \itemize{
#'   \item `{stat:n}` inserts the value of statistic `stat` rounded to `n`
#'     decimal places (trailing zeros kept), e.g. `{mean:2}` -> `50.35`.
#'   \item `{stat}` (no digits) inserts the value **verbatim** -- use this when
#'     the value is already a pre-formatted character string.
#' }
#' Everything outside the braces is copied literally, so a template such as
#' `"{mean:2} ({sd:3})"` yields `"50.35 (8.435)"` and `"{min:2}, {max:2}"` yields
#' `"40.23, 70.45"`.
#'
#' ## Numeric coercion
#' For a `{stat:n}` token the raw value is coerced to numeric before formatting.
#' If it cannot be coerced (it contains non-numeric characters) it is inserted
#' as-is, so partially pre-formatted input degrades gracefully rather than
#' erroring. A token whose statistic is absent for a group renders as an empty
#' string.
#'
#' @param data A data frame in long format. Grouping keys are all columns other
#'   than `stat_col` and `value_col`.
#' @param specs A data frame describing the output format: one `template_col`
#'   column plus one or more key columns (a subset of the grouping keys of
#'   `data`) used to join the template onto each row.
#' @param stat_col String. Name of the column in `data` holding the statistic
#'   name. Default `"stat"`.
#' @param value_col String. Name of the column in `data` holding the value.
#'   Default `"value"`.
#' @param out_col String. Name of the formatted output column to create. Default
#'   `"fmt_value"`.
#' @param template_col String. Name of the template column in `specs`. Default
#'   `"template"`.
#' @param digits Optional integer. Decimal places used for tokens that do *not*
#'   carry their own `:n`. Default `NULL` means a bare `{stat}` token is inserted
#'   verbatim.
#'
#' @return A data frame (tibble) with one row per group: the grouping-key columns
#'   followed by `out_col`. Rows keep the order in which each group first appears
#'   in `data`.
#'
#' @examples
#' df <- data.frame(
#'   column1 = "cohort1", column2 = "trt1", group1 = "Hoge",
#'   group2 = c("Age", "Age", "Age", "Age", "Age", "Age",
#'              "Sex", "Sex", "Sex", "Sex"),
#'   label = c("n", "Mean (SD)", "Mean (SD)", "Median",
#'             "Min, Max", "Min, Max", "Male", "Male", "Female", "Female"),
#'   stat = c("N", "mean", "sd", "median", "min", "max",
#'            "n", "p", "n", "p"),
#'   value = c(20, 50.345340, 8.435340, 48.453600, 40.234340, 70.445240,
#'             10, 50, 10, 50),
#'   stringsAsFactors = FALSE
#' )
#'
#' specs <- data.frame(
#'   group2 = c("Age", "Age", "Age", "Age", "Sex", "Sex"),
#'   label = c("n", "Mean (SD)", "Median", "Min, Max", "Male", "Female"),
#'   template = c("{N:0}", "{mean:2} ({sd:3})", "{median:2}",
#'                "{min:2}, {max:2}", "{n:0} ({p:1})", "{n:0} ({p:1})"),
#'   stringsAsFactors = FALSE
#' )
#'
#' format_stats(df, specs)
#'
#' @export
#' @importFrom dplyr left_join group_by across summarise arrange select all_of
#' @importFrom rlang .data :=
format_stats <- function(data,
                         specs,
                         stat_col = "stat",
                         value_col = "value",
                         out_col = "fmt_value",
                         template_col = "template",
                         digits = NULL) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  if (!is.data.frame(specs)) {
    stop("`specs` must be a data frame with a template column and key column(s).",
         call. = FALSE)
  }
  if (!stat_col %in% names(data)) {
    stop("`stat_col` = \"", stat_col, "\" is not a column of `data`.", call. = FALSE)
  }
  if (!value_col %in% names(data)) {
    stop("`value_col` = \"", value_col, "\" is not a column of `data`.", call. = FALSE)
  }
  if (!template_col %in% names(specs)) {
    stop("`template_col` = \"", template_col, "\" is not a column of `specs`.",
         call. = FALSE)
  }
  if (template_col %in% names(data)) {
    stop("`template_col` = \"", template_col,
         "\" already exists in `data`; choose a different name.", call. = FALSE)
  }
  if (!is.null(digits) &&
      (length(digits) != 1L || is.na(digits) || digits < 0)) {
    stop("`digits` must be NULL or a single non-negative integer.", call. = FALSE)
  }

  key_cols <- setdiff(names(specs), template_col)
  if (length(key_cols) == 0L) {
    stop("`specs` must contain at least one key column in addition to `",
         template_col, "`.", call. = FALSE)
  }
  missing_keys <- setdiff(key_cols, names(data))
  if (length(missing_keys)) {
    stop("`specs` key column(s) not found in `data`: ",
         paste(missing_keys, collapse = ", "), call. = FALSE)
  }

  id_cols <- setdiff(names(data), c(stat_col, value_col))
  if (out_col %in% id_cols) {
    stop("`out_col` = \"", out_col,
         "\" collides with a grouping-key column; choose a different name.",
         call. = FALSE)
  }

  order_col <- ".__format_stats_order__"
  data[[order_col]] <- seq_len(nrow(data))

  joined <- dplyr::left_join(data, specs, by = key_cols)

  out <- joined |>
    dplyr::group_by(dplyr::across(dplyr::all_of(id_cols))) |>
    dplyr::summarise(
      !!order_col := min(.data[[order_col]]),
      !!out_col := .format_stats_render(
        template = .data[[template_col]][1],
        stats = .data[[stat_col]],
        values = .data[[value_col]],
        digits = digits
      ),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data[[order_col]]) |>
    dplyr::select(-dplyr::all_of(order_col))

  out
}

#' Render a single template against one group's statistics
#'
#' Internal worker for [format_stats()]. Replaces `{stat}` / `{stat:n}` tokens in
#' `template` with the matching value from `stats`/`values`. Avoids gsub
#' back-references (unreliable in some R builds) by walking the match positions
#' returned by a Perl `gregexpr()` with capture groups.
#'
#' @param template Single template string (or `NA`).
#' @param stats Character vector of statistic names for the group.
#' @param values Vector of values aligned with `stats`.
#' @param digits Fallback decimals for tokens without an explicit `:n`.
#' @return A single formatted string, or `NA_character_` if `template` is `NA`.
#' @keywords internal
#' @noRd
.format_stats_render <- function(template, stats, values, digits = NULL) {
  if (length(template) != 1L || is.na(template)) {
    return(NA_character_)
  }
  template <- as.character(template)

  val_by_stat <- as.character(values)
  names(val_by_stat) <- as.character(stats)

  pattern <- "\\{([^{}:]+)(?::([0-9]+))?\\}"
  g <- gregexpr(pattern, template, perl = TRUE)[[1]]
  if (length(g) == 1L && g == -1L) {
    return(template)
  }

  starts <- as.integer(g)
  lengths <- attr(g, "match.length")
  cap_start <- attr(g, "capture.start")
  cap_len <- attr(g, "capture.length")

  pieces <- character(0)
  pos <- 1L
  for (i in seq_along(starts)) {
    s <- starts[i]
    if (s > pos) {
      pieces <- c(pieces, substr(template, pos, s - 1L))
    }
    name_s <- cap_start[i, 1L]
    name_l <- cap_len[i, 1L]
    stat_name <- trimws(substr(template, name_s, name_s + name_l - 1L))

    dig_l <- cap_len[i, 2L]
    if (dig_l > 0L) {
      dig_s <- cap_start[i, 2L]
      dig <- as.integer(substr(template, dig_s, dig_s + dig_l - 1L))
    } else {
      dig <- digits
    }

    raw <- if (stat_name %in% names(val_by_stat)) {
      val_by_stat[[stat_name]]
    } else {
      NA_character_
    }
    pieces <- c(pieces, .format_one_value(raw, dig))
    pos <- s + lengths[i]
  }
  nch <- nchar(template)
  if (pos <= nch) {
    pieces <- c(pieces, substr(template, pos, nch))
  }
  paste0(pieces, collapse = "")
}

#' Format a single statistic value for a template token
#'
#' Internal worker for [format_stats()]. Empty when the statistic is missing,
#' verbatim when `digits` is `NULL`, numeric-rounded when `digits` is given, and
#' left as-is when the value cannot be coerced to numeric.
#'
#' @param raw Single raw value (character, numeric or `NA`).
#' @param digits Decimals to apply, or `NULL` for verbatim insertion.
#' @return A single character string.
#' @keywords internal
#' @noRd
.format_one_value <- function(raw, digits) {
  if (length(raw) == 0L || is.na(raw)) {
    return("")
  }
  if (is.null(digits)) {
    return(as.character(raw))
  }
  num <- suppressWarnings(as.numeric(raw))
  if (is.na(num)) {
    return(as.character(raw))
  }
  sprintf(paste0("%.", digits, "f"), num)
}
