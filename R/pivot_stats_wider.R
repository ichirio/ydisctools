#' Expand one or more columns into combined column headers
#'
#' @description
#' `pivot_stats_wider()` reshapes a long summary table to wide form by spreading
#' one *or more* columns into the header. The spread columns are pasted together
#' (with `sep`) to form each new column name, and the cells are filled from
#' `value_col`. Every remaining column is kept as a row identifier. This is a
#' thin, opinionated wrapper around [tidyr::pivot_wider()] tuned for the
#' formatted statistic tables produced by [format_stats()].
#'
#' @details
#' For example, spreading `column1` and `column2` (values `cohort1`/`cohort2`
#' and `trt1`/`trt2`) with the default separator produces the headers
#' `cohort1____trt1`, `cohort1____trt2`, `cohort2____trt1`, `cohort2____trt2`.
#' The four-underscore default is chosen to be an unlikely substring of real
#' data, so the combined names can later be split back apart safely.
#'
#' @param data A data frame in long form.
#' @param expand_cols Character vector naming one or more columns to spread into
#'   the header. Their values (joined by `sep`) become the new column names.
#' @param value_col String. Name of the column whose values fill the new cells.
#'   Default `"fmt_value"`.
#' @param sep String used to join the `expand_cols` values into a header name.
#'   Default `"____"` (four underscores).
#' @param names_sort Logical. If `TRUE`, the new columns are sorted by name;
#'   if `FALSE` (default) they keep the order in which the combinations first
#'   appear in `data`.
#'
#' @return A data frame (tibble): the row-identifier columns (everything except
#'   `expand_cols` and `value_col`) followed by one column per spread
#'   combination.
#'
#' @examples
#' df <- data.frame(
#'   column1 = rep(c("cohort1", "cohort2"), each = 4),
#'   column2 = rep(rep(c("trt1", "trt2"), each = 2), times = 2),
#'   group1 = "Hoge",
#'   group2 = "Sex",
#'   label = rep(c("Male", "Female"), times = 4),
#'   fmt_value = "10 (50.0)",
#'   stringsAsFactors = FALSE
#' )
#'
#' pivot_stats_wider(df, expand_cols = c("column1", "column2"))
#'
#' @export
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr all_of
pivot_stats_wider <- function(data,
                              expand_cols,
                              value_col = "fmt_value",
                              sep = "____",
                              names_sort = FALSE) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  if (missing(expand_cols) || length(expand_cols) == 0L) {
    stop("`expand_cols` must name at least one column to spread.", call. = FALSE)
  }
  if (length(value_col) != 1L) {
    stop("`value_col` must be a single column name.", call. = FALSE)
  }
  missing_cols <- setdiff(c(expand_cols, value_col), names(data))
  if (length(missing_cols)) {
    stop("Column(s) not found in `data`: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  id_cols <- setdiff(names(data), c(expand_cols, value_col))

  tidyr::pivot_wider(
    data,
    id_cols = dplyr::all_of(id_cols),
    names_from = dplyr::all_of(expand_cols),
    values_from = dplyr::all_of(value_col),
    names_sep = sep,
    names_sort = names_sort
  )
}
