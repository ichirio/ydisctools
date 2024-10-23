#' Check for Invalid Characters in a Data Frame(SDTM)
#'
#' This function checks for invalid characters in a specified column of a data frame and provides various transformations of the invalid characters.
#'
#' @param df A data frame in which to check for invalid characters.
#' @param target A symbol specifying the column to check for invalid characters. Default is `text`.
#' @param question A logical value indicating whether to detect and remove the question mark character. Default is FALSE.
#' @return A data frame with additional columns showing the invalid characters and their transformations.
#' @examples
#' library(dplyr)
#' df <- data.frame(text = c("Hello\x01World", "Example?String"))
#' check_invalid_chars(df, target = text)
#' check_invalid_chars(df, target = text, question = TRUE)
#' @import dplyr
#' @export
check_invalid_chars <- function(df, target=text, question=FALSE) {
  result <- df |>
    mutate(
      inv_chars      = detect_invalid_chars({{target}}, question),
      inv_chars_hex  = detect_invalid_chars({{target}}, question) |> to_hex(),
      inv_chars_ctrl = detect_invalid_chars({{target}}, question) |> replace_control_chars(),
      inv_if_latin1  = detect_invalid_chars({{target}}, question) |> iconv(from = "latin1", to = "UTF-8"),
      inv_if_sjis    = detect_invalid_chars({{target}}, question) |> iconv(from = "SJIS", to = "UTF-8")
    )
  return(result)
}


#' Detect Invalid Characters for SDTM (non-ascii + alpha)
#'
#' This function detects invalid characters for SDTM from a given text. It can optionally detect the question mark character, it can be used for broken characters on SAS.
#'
#' @param text A character string from which invalid characters are to be removed.
#' @param question A logical value indicating whether to detect and remove the question mark character. Default is FALSE.
#' @return A character string with invalid characters removed.
#' @examples
#' detect_invalid_chars("Hello\x01World")
#' detect_invalid_chars("Hello?World", question = TRUE)
#' @export
detect_invalid_chars <- function(text, question = FALSE) {
  expression <- "[\\x20-\\x7E]"
  if(question) {
    expression <- "[\\x20-\\x3E\\x40-\\x7E]"
  }
  return(gsub(expression, "", text, perl = TRUE, useBytes = TRUE))
}


#' Convert a String to Hexadecimal Representation
#'
#' This function converts each character in a string to its hexadecimal representation.
#'
#' @param text A character string to be converted to hexadecimal representation.
#' @return A character vector where each element is the hexadecimal representation of the corresponding character in the input string.
#' @examples
#' to_hex("Hello")
#' to_hex("World")
#' @import purrr
#' @export
to_hex <- function(text) {
  return(map_chr(text, ~ paste(charToRaw(.), collapse = " ")))
}


#' Replace Control Characters with Descriptive Names
#'
#' This function replaces control characters in a given text with their descriptive names.
#'
#' @param text A character string in which control characters are to be replaced.
#' @return A character string with control characters replaced by their descriptive names.
#' @examples
#' replace_control_chars("Hello\x01World")
#' replace_control_chars("Example\x0AString")
#' @export
replace_control_chars <- function(text) {
  control_char_map <- c(
    "\\x00" = "[NUL]", "\\x01" = "[SOH]", "\\x02" = "[STX]", "\\x03" = "[ETX]",
    "\\x04" = "[EOT]", "\\x05" = "[ENQ]", "\\x06" = "[ACK]", "\\x07" = "[BEL]",
    "\\x08" = "[BS]",  "\\x09" = "[TAB]", "\\x0A" = "[LF]",  "\\x0B" = "[VT]",
    "\\x0C" = "[FF]",  "\\x0D" = "[CR]",  "\\x0E" = "[SO]",  "\\x0F" = "[SI]",
    "\\x10" = "[DLE]", "\\x11" = "[DC1]", "\\x12" = "[DC2]", "\\x13" = "[DC3]",
    "\\x14" = "[DC4]", "\\x15" = "[NAK]", "\\x16" = "[SYN]", "\\x17" = "[ETB]",
    "\\x18" = "[CAN]", "\\x19" = "[EM]",  "\\x1A" = "[SUB]", "\\x1B" = "[ESC]",
    "\\x1C" = "[FS]",  "\\x1D" = "[GS]",  "\\x1E" = "[RS]",  "\\x1F" = "[US]",
    "\\x7F" = "[DEL]"
  )

  for (char in names(control_char_map)) {
    text <- gsub(char, control_char_map[char], text, perl = TRUE, useBytes = TRUE)
  }
  return(text)
}

