#' Check SDTM Keys
#'
#' This function checks the key variables in SDTM datasets against the key variables metadata.
#'
#' @param sdtms A list of SDTM data.frames.
#' @param dataset_meta A data.frame containing dataset and key variables metadata.
#' @return A data.frame with the results of the key checks.
#' @examples
#' \dontrun{
#' sdtms <- list(ds1 = data.frame(ID = 1:3), ds2 = data.frame(ID = 1:2))
#' dataset_meta <- data.frame(dataset = c("ds1", "ds2"), key = c("ID", "ID"))
#' check_sdtm_keys(sdtms, dataset_meta)
#' }
#' @import dplyr
#' @import purrr
#' @export
check_sdtm_keys <- function(sdtms, dataset_meta) {
  # sdtms is a list of SDTM data.frames
  # dataset_meta is a data.frame having dataset, key variables

  # check if the sdtms is a list
  if (!is.list(sdtms)) {
    stop("sdtms must be a list of data.frames")
  }

  # check if dataset_meta has the required columns (ignore case)
  if (!all(c("dataset", "key") %in% tolower(colnames(dataset_meta)))) {
    stop("dataset_meta must have 'dataset' and 'key' columns")
  }

  result <- names(sdtms) |>
    map(~ {
      ds <- sdtms[[.x]]

      # Skip if ds is not a data.frame
      if (!is.data.frame(ds)) {
        return(NULL)
      }

      #
      key_vec <- dataset_meta |>
        filter(tolower(dataset) == tolower(.x)) |>
        pull(key) |>
        split_key_text()

      if(length(key_vec) == 0 || all(is.na(key_vec))) {
        rdf <- data.frame(dataset = .x,
                          key = NA,
                          dup = NA,
                          description = "ERROR: Key variables not found for dataset")
        return(rdf)
      }

      if (!all(key_vec %in% colnames(ds))) {
        rdf <- data.frame(dataset = .x,
                          key = paste(key_vec, collapse = ", "),
                          dup = NA,
                          description = "ERROR: Key variables not found in the data frame")
        return(rdf)
      }

      rdf <- ds %>%
        mutate(OBS = row_number()) |>
        group_by(across(all_of(key_vec))) |>
        filter(n() > 1) |>
        mutate(description = paste(OBS, collapse = ":")) |>
        distinct(across(all_of(key_vec)), description) |>
        ungroup() |>
        mutate(dataset = .x,
               key = paste(key_vec, collapse = ", "),
               description = paste(description, collapse = " "),
               dup = n()) |>
        select(dataset, key, dup, description) |>
        slice(1)

      if(nrow(rdf) == 0) {
        rdf <- data.frame(dataset = .x,
                          key = paste(key_vec, collapse = ", "),
                          dup = 0,
                          description = "No duplicates found")
      }

      return(rdf)
    })

  return(bind_rows(result))
}

#' Split Key Text
#'
#' This function splits a key text into individual key components.
#'
#' @param key_text A character string containing key variables separated by delimiters.
#' @return A character vector of key variables.
#' @examples
#' split_key_text("ID, NAME")
#' @export
split_key_text <- function(key_text) {
  if(is.null(key_text) || length(key_text) == 0 || is.na(key_text) || key_text == "") {
    return(NA)
  }

  result <- key_text |>
    strsplit("[ ,;:\r\n\t]") |>
    unlist() |>
    trimws()

  result <- result[result != ""]

  return(result)
}



#' Check if Key is Unique
#'
#' This function checks if the key variables form a unique key in the data.frame.
#'
#' @param df A data.frame to check.
#' @param key A character vector of key variables.
#' @return TRUE if the key is unique, FALSE otherwise.
#' @examples
#' df <- data.frame(ID = c(1, 2, 2), NAME = c("A", "B", "B"))
#' is_unique_key(df, c("ID", "NAME"))
#' @export
is_unique_key <- function(df, key) {
  if (!all(key %in% colnames(df))) {
    stop("key columns not found in the data frame")
  }

  out <- df %>%
    mutate(OBS = row_number()) |>
    group_by(across(all_of(key))) |>
    filter(n() > 1) |>
    mutate(description = paste(OBS, collapse = ":")) |>
    ungroup()

  return(nrow(out) == 0)
}
