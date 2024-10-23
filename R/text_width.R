#' Calculate the Width of a String in a Specified Font
#'
#' This function estimates the width of a given string when rendered in a specified font at a specified point size.
#'
#' @param text A character string whose width is to be estimated.
#' @param font A character string specifying the font to be used. Supported fonts are 'Arial' and 'Courier New'. Default is 'Arial'.
#' @param points A numeric value representing the font size in points. Default is 12.
#' @param bold A logical value indicating whether the text is bold. Default is FALSE.
#' @return A numeric value representing the estimated width of the string in points.
#' @examples
#' text_width("AETERM")
#' text_width("AE (Adverse Events)", font = "Courier New", points = 16, bold = TRUE)
#' @export
text_width <- function(text, font = "Arial", points = 12, bold = FALSE) {
  if (font == "Arial") {
    return(text_width_arial(text, points, bold))
  } else if (font == "Courier New") {
    return(text_width_courier_new(text, points, bold))
  } else {
    stop("Unsupported font. Supported fonts are 'Arial' and 'Courier New'.")
  }
}

#' Calculate the Width of a String in Arial Font
#'
#' This function estimates the width of a given string when rendered in Arial font at a specified point size.
#'
#' @param text A character string whose width is to be estimated.
#' @param points A numeric value representing the font size in points. Default is 12.
#' @param bold A logical value indicating whether the text is bold. Default is FALSE.
#' @return A numeric value representing the estimated width of the string in points.
#' @examples
#' text_width_arial("AETERM")
#' text_width_arial("AE (Adverse Events)", points = 16, bold = TRUE)
#' @export
text_width_arial <- function(text, points = 12, bold = FALSE) {
  char_widths <- c(
    "A" = 8.00, "B" = 8.00, "C" = 8.80, "D" = 8.65, "E" = 8.05, "F" = 7.40, "G" = 9.35, "H" = 8.70,
    "I" = 3.40, "J" = 6.05, "K" = 8.00, "L" = 6.65, "M" = 10.00, "N" = 8.65, "O" = 9.45, "P" = 8.00,
    "Q" = 9.35, "R" = 8.80, "S" = 8.00, "T" = 7.30, "U" = 8.70, "V" = 8.10, "W" = 11.45, "X" = 8.15,
    "Y" = 8.00, "Z" = 7.45, "a" = 6.70, "b" = 6.75, "c" = 6.00, "d" = 6.65, "e" = 6.75, "f" = 3.35,
    "g" = 6.70, "h" = 6.75, "i" = 2.65, "j" = 2.75, "k" = 6.05, "l" = 2.70, "m" = 10.10, "n" = 6.65,
    "o" = 6.65, "p" = 6.75, "q" = 6.70, "r" = 4.05, "s" = 5.95, "t" = 3.30, "u" = 6.75, "v" = 5.95,
    "w" = 8.75, "x" = 6.00, "y" = 6.00, "z" = 6.00, "0" = 6.70, "1" = 6.70, "2" = 6.70, "3" = 6.70,
    "4" = 6.70, "5" = 6.70, "6" = 6.70, "7" = 6.70, "8" = 6.70, "9" = 6.70, " " = 3.40, "=" = 7.05,
    "-" = 4.00, "+" = 7.00, "_" = 6.85, "/" = 3.30, "?" = 6.65, "!" = 3.45, "\"" = 4.30, "'" = 2.35,
    "(" = 3.95, ")" = 3.95, "[" = 3.40, "]" = 3.40, "|" = 3.15, "<" = 7.05, ">" = 7.05, "." = 3.40,
    "," = 3.40
  )
  default_width <- 6.00

  ## If length(text) > 1, return an error, because the function can support a single character
  if (length(text) > 1) stop("text must be a single character.")

  ## If the text is NA or NULL, return NA
  if (is.na(text) || is.null(text)) return(NA)

  width <- 0
  for (char in strsplit(text, NULL)[[1]]) {
    if (char %in% names(char_widths)) {
      width <- width + char_widths[[char]]
    } else {
      warning(paste("Character", char, "not found in width table."))
      width <- width + default_width
    }
  }
  if(bold) width <- width * 1.09
  if(points > 0) width <- width * points / 12
  return(round(width, 3))
}

#' Calculate the Width of a String in Courier New Font
#'
#' This function estimates the width of a given string when rendered in Courier New font at a specified point size.
#'
#' @param text A character string whose width is to be estimated.
#' @param points A numeric value representing the font size in points. Default is 12.
#' @param bold A logical value indicating whether the text is bold. Default is FALSE.
#' @return A numeric value representing the estimated width of the string in points.
#' @examples
#' text_width_courier_new("AETERM")
#' text_width_courier_new("AE (Adverse Events)", points = 16, bold = TRUE)
#' @export
text_width_courier_new <- function(text, points = 12, bold = FALSE) {
  char_width <- 7.22  # Courier New font 12 points width

  ## If length(text) > 1, return an error, because the function can support a single character
  if (length(text) > 1) stop("text must be a single character.")

  ## If the text is NA or NULL, return NA
  if (is.na(text) || is.null(text)) return(NA)

  width <- nchar(text) * char_width

  if(bold) width <- width * 1.09
  if(points > 0) width <- width * points / 12
  return(round(width, 3))
}



