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
