#' Compare Character Dates
#'
#' This function compares two character dates based on a specified comparison operator.
#'
#' @param x A character string representing the first date.
#' @param y A character string representing the second date.
#' @param mark A character string representing the comparison operator.
#'             Must be one of "<", "<=", ">", ">=", "==", "!=". Default is "<=".
#' @return A logical value indicating the result of the comparison, or NA if the input is invalid.
#' @examples
#' comp_char_date("2023-10-01", "2023-10-02", "<=") # TRUE
#' comp_char_date("2023-10-01", "2023-10-02", ">")  # FALSE
#' comp_char_date("2023-10-01", "2023-10-01", "==") # TRUE
#'
#' @import stringr
#' @export
comp_char_date <- function(x, y, mark = "<=") {
  if(is.null(x) | is.null(y)) return(NA)
  if(!(mark %in% c("<", "<=", ">", ">=", "==", "!="))) return(NA)

  len <- min(str_length(x), str_length(y))
  if(len < 4) return(NA)

  x_sub <- str_sub(x, 1, len)
  y_sub <- str_sub(y, 1, len)

  return(eval(parse(text = paste("x_sub", mark, "y_sub"))))
}


#' Compare Character Date Vectors
#'
#' This function compares two character date vectors element-wise based on a specified comparison operator.
#'
#' @param x A character vector representing the first set of dates.
#' @param y A character vector representing the second set of dates.
#' @param mark A character string representing the comparison operator.
#'             Must be one of "<", "<=", ">", ">=", "==", "!=". Default is "<=".
#' @return A logical vector indicating the result of the comparison for each element, or an error if the input vectors have different lengths.
#' @examples
#' x <- c("2023-10-01", "2023-10-02")
#' y <- c("2023-10-01", "2023-10-03")
#' comp_char_dates(x, y, "<=") # TRUE, TRUE
#' comp_char_dates(x, y, ">")  # FALSE, FALSE
#'
#' @import purrr
#' @export
comp_char_dates <- function(x, y, mark = "<=") {
  if(length(x) != length(y)) stop("The input vectors must have the same length.")

  return(map_lgl(seq_along(x), ~ comp_char_date(x[.x], y[.x], mark)))
}
