#' Compare Character Dates
#'
#' This function compares two character dates based on a specified comparison operator.
#'
#' @param x A character string representing the first date.
#' @param y A character string representing the second date.
#' @param mark A character string representing the comparison operator.
#'             Must be one of "<", "<=", ">", ">=", "==", "!=". Default is "<=".
#' @param mode A character string representing the comparison mode.
#'             Must be one of "S", "IMAX", "IMIN". Default is "<=".
#'             S: Cmpare the dates with shorter length.
#'             IMAX: Cmpare the dates imputing as max value. e.g.) 2025-01 can be imputed as 2025-01-99
#'             IMIN: Cmpare the dates imputing as min value. e.g.) 2025-01 can be imputed as 2025-01-00
#' @return A logical value indicating the result of the comparison, or NA if the input is invalid.
#' @examples
#' comp_char_date("2023-10-01", "2023-10-02", "<=") # TRUE
#' comp_char_date("2023-10-01", "2023-10-02", ">")  # FALSE
#' comp_char_date("2023-10-01", "2023-10-01", "==") # TRUE
#'
#' @import stringr
#' @export
comp_char_date <- function(x, y, mark = "<=", mode = "S") {
  if(is.null(x) | is.null(y)) return(NA)
  if(is.na(x) | is.na(y)) return(NA)
  if(!(mark %in% c("<", "<=", ">", ">=", "==", "!="))) return(NA)

  if(mode %in% c("IMAX", "IMIN")) {
    add_char <- if(mode == "IMAX") "Z" else "'"
    if(str_length(x) > str_length(y)) {
      x_sub <- x
      y_sub <- paste0(y, add_char)
    }else if(str_length(x) < str_length(y)) {
      x_sub <- paste0(x, add_char)
      y_sub <- y
    }else {
      x_sub <- x
      y_sub <- y
    }
  } else {
    len <- min(str_length(x), str_length(y))
    if(len < 4) return(NA)
    x_sub <- str_sub(x, 1, len)
    y_sub <- str_sub(y, 1, len)
  }

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
#' @param mode A character string representing the comparison mode.
#'             Must be one of "S", "IMAX", "IMIN". Default is "<=".
#'             S: Cmpare the dates with shorter length.
#'             IMAX: Cmpare the dates imputing as max value. e.g.) 2025-01 can be imputed as 2025-01-99
#'             IMIN: Cmpare the dates imputing as min value. e.g.) 2025-01 can be imputed as 2025-01-00
#' @return A logical vector indicating the result of the comparison for each element, or an error if the input vectors have different lengths.
#' @examples
#' x <- c("2023-10-01", "2023-10-02")
#' y <- c("2023-10-01", "2023-10-03")
#' comp_char_dates(x, y, "<=") # TRUE, TRUE
#' comp_char_dates(x, y, ">")  # FALSE, FALSE
#'
#' @import purrr
#' @export
comp_char_dates <- function(x, y, mark = "<=", mode = "S") {
  if(length(x) != length(y)) stop("The input vectors must have the same length.")

  return(map_lgl(seq_along(x), ~ comp_char_date(x[.x], y[.x], mark, mode)))
}
