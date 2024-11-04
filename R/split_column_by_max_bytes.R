#' Split a specified column into multiple columns based on maximum byte length
#'
#' @param df A data frame containing the column to be split.
#' @param var_name A string specifying the name of the column to be split.
#' @param max_bytes An integer specifying the maximum byte length for each split. Default is 200.
#' @param keep_org A logical value indicating whether to keep the original column. Default is FALSE.
#' @param label A string specifying a label. Must be a character and less than 40 characters. Default is NULL.
#' @param splitter A regular expression pattern used to identify split points. Default is " |;|,|\\.".
#' @return A data frame with the specified column split into multiple columns.
#' @examples
#' df <- data.frame(
#'   id = 1:3,
#'   text = c("This is a very long text.", "This is another long text.", "Short text")
#' )
#' result <- split_column_by_max_bytes(df, "text", max_bytes = 10)
#' print(result)
#' @importFrom purrr map
#' @importFrom stringi stri_locate_all_regex stri_sub
#' @export
split_column_by_max_bytes <- function(df, var_name, max_bytes = 200, label = NULL,
                                      keep_org = FALSE, splitter = " |;|,|\\.|-") {
  if(!is.data.frame(df)) {
    stop("df must be a data.frame")
  }

  if(!var_name %in% names(df)) {
    stop("var_name must be in df")
  }

  if(!is.na(label) && is.character(label) && nchar(label) > 40) {
    stop("label must be a character and less than 40 characters")
  }

  # Keep original text
  if(keep_org) original_text <- df[[var_name]]

  # Split texts
  sep_texts <- map(df[[var_name]], ~ split_text_by_max_bytes(., max_bytes = max_bytes, splitter = splitter))

  # Get max number of split texts
  max_len <- max(sapply(sep_texts, length))

  # Get variable names
  var_names <- get_split_var_names(var_name, max_len)

  ## Make variables on df, according to var_name_base + sequential number
  for(i in seq_len(max_len)) {
    df[[var_names[i]]] <- sapply(sep_texts, function(x) x[i])
    if(!is.null(label)) attr(df[[var_names[i]]], "label") <- label
  }

  # Keep original variable
  if(keep_org) df[[paste0(var_name, "_ORG")]] <- original_text

  return(df)
}



#' Split a text into multiple parts based on maximum byte length
#'
#' This function splits a given text into multiple parts, ensuring that each part does not exceed the specified maximum byte length. It also ensures that the text is not split in the middle of a multibyte character.
#'
#' @param text A character string to be split.
#' @param max_bytes An integer specifying the maximum byte length for each split. Default is 200.
#' @param splitter A regular expression pattern used to identify split points. Default is " |;|,|\\.".
#' @return A character vector with the text split into parts.
#' @examples
#' text <- "This is a very long text that needs to be split into smaller parts."
#' result <- split_text_by_max_bytes(text, max_bytes = 10)
#' print(result)
#' @importFrom stringi stri_sub
#' @export
split_text_by_max_bytes <- function(text, max_bytes = 200, splitter = " |;|,|\\.|-") {
  len_bytes <- nchar(text, type = "bytes")
  if(is.null(text) || is.na(text) || !is.character(text)) {
    return(NA_character_)
  }
  if(len_bytes == 0) {
    return("")
  }
  if(len_bytes <= max_bytes) {
    return(text)
  }

  result <- c()
  single_byte <- FALSE
  len_char <- nchar(text)
  start_pos <- 1
  end_pos <- 1
  for(i in seq_len(len_char)) {
    current_char <- substr(text, i, i)
    is_splitter <- grepl(splitter, current_char)
    is_single_byte <- nchar(current_char, type = "bytes") == 1
    target <- substr(text, start_pos, i)
    target_bytes <- nchar(target, type = "bytes")

    if(!is_single_byte || is_splitter) {
      if(single_byte) {
        end_pos <- i - 1
      }
      single_byte <- FALSE
    }else {
      single_byte <- TRUE
    }

    if(target_bytes > max_bytes) {
      tmp <- substring(text, start_pos, end_pos)
      tmp_bytes <- nchar(tmp, type = "bytes")
      if(tmp_bytes < max_bytes/2) {
        end_pos <- i - 1
        tmp <- substring(text, start_pos, end_pos)
      }

      result <- c(result, tmp)
      start_pos <- end_pos + 1
      end_pos <- i
    }

    if(is_splitter) {
      end_pos <- i
      single_byte <- FALSE
    }else if(!is_single_byte) {
      end_pos <- i
      single_byte <- FALSE
    }

    if(i == len_char) {
      tmp <- substring(text, start_pos, i)
      if(nchar(tmp) > 0) {
        result <- c(result, tmp)
      }
    }
  }
  result <- result[result != ""]
  return(result)
}

#' Generate split variable names based on a base variable name
#'
#' This function generates a list of variable names for split columns based on a base variable name.
#' The names are generated to ensure that they fit within a specified maximum length.
#'
#' @param var_name The base name of the variable to be split.
#' @param len The number of split variables to generate names for.
#' @param max_len The maximum length of the variable names. Default is the length of the base variable name.
#' @return A character vector containing the generated variable names.
#' @examples
#' var_names <- get_split_var_names("variable", 5)
#' print(var_names)
#' @export
get_split_var_names <- function(var_name, len, max_len = len) {
  if(max_len - 1 >= 10) {
    var_name_base <- substr(var_name, 1, 6)
  }else {
    var_name_base <- substr(var_name, 1, 7)
  }

  var_names <- c()
  for(i in seq_len(len)) {
    if(i == 1) {
      var_names <- c(var_names, var_name)
    }else {
      if(max_len - 1 < 10) {
        var_names <- c(var_names, paste0(var_name_base, i - 1))
      }else {
        var_names <- c(var_names, paste0(var_name_base, sprintf("%02d", i - 1)))
      }
    }
  }
  return(var_names)
}

#' Set a label on split columns based on specified column in a data frame
#'
#' This function sets a label on split columns based on specified column in a data frame. The columns are identified based on a base variable name followed by one or two digits.
#'
#' @param df A data frame containing the columns to be labeled.
#' @param var_name The base name of the variable to be labeled.
#' @param label A character string specifying the label to be set. Must be less than 40 characters. Default is the label attribute of the first specified column.
#' @return The data frame with the label attribute set on the specified columns.
#' @examples
#' df <- data.frame(
#'   id = 1:3,
#'   text1 = c("This is a test.", "Another test.", "Short text"),
#'   text2 = c("More text.", "Even more text.", "Tiny text")
#' )
#' df <- set_label_on_sep_columns(df, "text", label = "Example Label")
#' print(attr(df$text1, "label"))
#' print(attr(df$text2, "label"))
#' @export
set_label_on_sep_columns <- function(df, var_name, label = attr(df[[var_name]], "label")) {
  if(!is.data.frame(df)) {
    stop("df must be a data.frame")
  }

  if(!is.character(label) || nchar(label) > 40) {
    stop("label must be a character and less than 40 characters")
  }

  pattern1 <- paste0(substring(var_name, 1, 7), "\\d{1}$")
  pattern2 <- paste0(substring(var_name, 1, 6), "\\d{2}$")
  pattern  <- paste0(pattern1, "|", pattern2)
  var_names <- grep(pattern, names(df), value = TRUE)

  for(var_name in var_names) {
    attr(df[[var_name]], "label") <- label
  }

  return(df)
}

