#' Check for Invalid Characters in Multiple Datasets
#'
#' This function checks for invalid characters in multiple datasets and combines the results into a single data frame.
#'
#' @param datasets A list of data frames in which to check for invalid characters.
#' @param question A logical value indicating whether to detect and remove the question mark character. Default is FALSE.
#' @return A data frame with the results of the invalid character checks for all datasets combined.
#' @examples
#' library(dplyr)
#' library(purrr)
#' df1 <- data.frame(text = c("Hello\x01World", "Example\x0AString"))
#' df2 <- data.frame(text = c("Another\x0DTest", "More\x0AData"))
#' datasets <- list(df1, df2)
#' check_invalid_chars_in_datasets(datasets)
#' check_invalid_chars_in_datasets(datasets, question = TRUE)
#' @import dplyr
#' @import purrr
#' @import readr
#' @export
check_invalid_chars_in_datasets <- function(datasets, question=FALSE, report_file = NULL) {
  result <- map2(datasets, names(datasets), ~ check_invalid_chars(.x, .y, question = question)) |>
    bind_rows()

  if(!is.null(report_file)) {
    write_csv(result, report_file)
  }
  return(result)
}

#' Check for Invalid Characters in a Data Frame(SDTM)
#'
#' This function checks for invalid characters in a specified column of a data frame and provides various transformations of the invalid characters.
#'
#' @param df A data frame in which to check for invalid characters.
#' @param df_name A data frame name to be used in a report. Default is the name of the input data frame.
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
check_invalid_chars <- function(df, df_name = deparse(substitute(df)), target=text, question=FALSE) {
  result <- df |>
    convert_df_longer() |>
    mutate(dataset = df_name, .before = 1) |>
    mutate(inv_chars      = detect_invalid_chars({{target}}, question)) |>
    filter(inv_chars != "") |>
    mutate(
      inv_chars_hex  = to_hex(inv_chars),
      inv_chars_ctrl = replace_control_chars(inv_chars),
      inv_chars_positions = get_char_positions({{target}}, inv_chars),
      inv_if_latin1  = iconv(inv_chars, from = "latin1", to = "UTF-8"),
      inv_if_sjis    = iconv(inv_chars, from = "SJIS", to = "UTF-8")
    )

  return(result)
}

convert_df_longer <- function(df) {
  result <- df |>
    select_if(is.character) |>
    mutate(row_no = row_number()) |>
    pivot_longer(cols = -row_no, names_to = "variable_name", values_to = "text")

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
#to_hex <- function(text) {
#  return(map_chr(text, ~ paste(charToRaw(.), collapse = " ")))
#}
to_hex <- function(text) {
  return(map_chr(text, ~ to_hex2(.)))
}

to_hex2 <- function(text) {
  if (length(text) == 0) {
    return(text)
  }else if(length(text) != 1){
    stop("Input must be a single character")
  }
  chars <- strsplit(text, NULL)[[1]]

  hex_codes <- map_chr(chars, ~ paste(charToRaw(.), collapse = ":"))

  spaced_hex <- paste(hex_codes, collapse = " ")

  return(spaced_hex)
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

get_char_positions <- function(text, chars) {
  if(!is.null(text) && !is.null(chars) && is.character(text) && is.character(chars) &&
     length(text) == 1 && length(chars) == 1 && str_length(text) > 0 && str_length(chars) > 0) {
    char_vec <- unique(unlist(strsplit(chars, split = "")))

    all_positions <- unlist(map(char_vec, ~ {
      positions <- gregexpr(., text)[[1]]
      positions[positions > 0]
    }))
    return(sort(unique(all_positions)))
  }
  return("")
}


