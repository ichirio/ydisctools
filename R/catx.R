#' Concatenate strings with a separator, excluding empty strings and NA values
#'
#' `catx` concatenates multiple vectors or character strings using a specified separator, while
#' excluding any empty strings (`""`) and `NA` values.
#'
#' @param sep A character string used to separate the concatenated values.
#' @param ... Vectors or character strings to concatenate. Empty strings and `NA` values are ignored.
#' @return A character vector where each element is a concatenation of the input strings, separated by `sep`.
#' @details This function is useful when you want to concatenate multiple values but avoid including
#' empty strings or `NA` values in the result. The function internally uses `paste` to handle
#' string concatenation and `pmap_chr` to iterate over multiple inputs.
#' @examples
#' # Basic usage
#' catx("", "Hello", "", NA, "World")
#' # Returns: "HelloWorld"
#'
#' # Using a space as a separator
#' catx(" ", "Hello", "", NA, "World")
#' # Returns: "Hello World"
#'
#' # Concatenating multiple vectors with commas
#' catx(",", c("A", NA), c("", "B"), "C")
#' # Returns: "A,C" "B"
#' @export
#' @import purrr
catx <- function(sep, ...) {
  pmap_chr(list(...), ~ paste(c(...)[c(...) != "" & !is.na(c(...))], collapse = sep))
}
