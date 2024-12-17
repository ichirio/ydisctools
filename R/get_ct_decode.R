#' Get Decode Value from CT Metadata
#'
#' This function retrieves the decode value from a metadata data.frame based on the specified ID and term.
#'
#' @param ct_meta A data.frame containing the metadata with columns `id`, `term`, and `decode`. read_sdtm_metadata_p21() can be used to read the codelists metadata from a P21 specification file.
#' @param id A single character string representing the ID.
#' @param term A character vector representing the term(s).
#' @param reverse A logical value indicating whether to reverse the lookup (default is FALSE).
#'
#' @return A character vector of decode values corresponding to the specified ID and term.
#'
#' @examples
#' library(dplyr)
#'
#' # Create a sample metadata data.frame
#' ct_meta <- data.frame(
#'   id = c("LATESTCD", "LBTESTCD", "LBTESTCD"),
#'   term = c("RBC", "MONO", "LYM"),
#'   decode = c("Erythrocyte Count", "Monocyte Count", "Lymphocyte Count"),
#'   stringsAsFactors = FALSE
#' )
#'
#' df <- data.frame(USUBJID = rep(1:3), LBTESTCD = c("RBC", "MONO", "LYM"))
#'
#' # Get decode value
#' df |> mutate(LBTEST = get_ct_decode(ct_meta, "LBTESTCD", LBTESTCD))
#'
#' df2 <- data.frame(USUBJID = rep(1:3), LBTEST = c("Erythrocyte Count", "Monocyte Count", "Lymphocyte Count"))
#'
#' # Get decode value with reverse lookup
#' df2 |> mutate(LBTEST = get_ct_decode(ct_meta, "LBTESTCD", LBTESTCD, reverse = TRUE))
#'
#' @import dplyr
#' @export
get_ct_decode <- function(ct_meta, id, term, reverse = FALSE) {
  # Check if ct_meta is a data.frame with columns id, term, decode
  if (!all(c("id", "term", "decode") %in% names(ct_meta))) {
    stop("ct_meta must be a data.frame with columns id, term, decode")
  }

  # id is a single character string
  if (!is.character(id) && length(id) != 1) {
    stop("id must be a character vector with length 1")
  }

  # term is a character vector
  if (!is.character(term)) {
    stop("term must be a character vector")
  }else if (length(term) == 0) {
    return(character(0))
  }

  if(reverse) {
    data.frame(id = id, decode = term) |>
      left_join(ct_meta, by = c("id", "decode")) |>
      pull(term)
  }else {
    data.frame(id = id, term = term) |>
      left_join(ct_meta, by = c("id", "term")) |>
      pull(decode)
  }
}

