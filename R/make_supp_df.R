#' Create a suppqual DataFrame
#'
#' @description
#' This function creates a suppqual dataframe using the specified dataframe and suppqual metadata dataframe. AP domains are not supported.
#'
#' @param df DataFrame. The original sdtm dataframe with supp variables.
#' @param suppmeta DataFrame. The dataframe containing supp variables metadata. The suppmeta have to have the following columns: RDOMAIN, IDVAR, QNAM, QLABEL, QORIG, QEVAL.
#' @param idver String. To replace IDVAR provided in suppmeta. (optional).
#'
#' @return DataFrame. The supplementary dataframe.
#'
#' @import dplyr
#' @import tidyr
#' @export
#'
#' @examples
#' dm <- data.frame(
#'   STUDYID = c("HOGE-01", "HOGE-01", "HOGE-01"),
#'   DOMAIN = c("DM", "DM", "DM"),
#'   USUBJID = c("HOGE-01-001", "HOGE-01-002", "HOGE-01-003"),
#'   SUBJID = c("001", "002", "003"),
#'   SUPP1 = c("ASIAN", "ASIAN", ""),
#'   SUPP2 = c("WHITE", "", ""),
#'   SUPP3 = c("", "", "BLACK OR AFRICAN AMERICAN")
#' )
#'
#' ae <- data.frame(
#'   STUDYID = c("HOGE-01", "HOGE-01", "HOGE-01"),
#'   DOMAIN = c("AE", "AE", "AE"),
#'   USUBJID = c("HOGE-01-001", "HOGE-01-002", "HOGE-01-003"),
#'   AESEQ = c(1, 20, 30),
#'   AETERM = c("term1", "term2", "term3"),
#'   SUPP1 = c("supp1a", "supp1b", ""),
#'   SUPP2 = c("supp2a", "", ""),
#'   SUPP3 = c("supp3a", "supp3b", NA)
#' )
#'
#' apcm <- data.frame(
#'   STUDYID = c("HOGE-01", "HOGE-01", "HOGE-01"),
#'   DOMAIN = c("APCM", "APCM", "APCM"),
#'   APID = c("HOGE-01-001-P", "HOGE-01-002-P", "HOGE-01-003-P"),
#'   CMSPID = c("CM-001", "CM-002", "CM-003"),
#'   CMTRT = c("trt1", "trt2", "trt3"),
#'   SUPP1 = c("supp1a", "supp1b", "supp1c"),
#'   SUPP2 = c("supp2a", "supp2b", ""),
#'   SUPP3 = c("", "", "")
#' )
#'
#' suppmeta <- data.frame(
#'   RDOMAIN = c("DM", "DM", "DM", "AE", "AE", "AE", "APCM", "APCM", "APCM"),
#'   IDVAR = c("", "", "", "AESEQ", "AESEQ", "AESEQ", "CMSPID", "CMSPID", "CMSPID"),
#'   QNAM = c("SUPP1", "SUPP2", "SUPP3", "SUPP1", "SUPP2", "SUPP3", "SUPP1", "SUPP2", "SUPP3"),
#'   QLABEL = c("Supp Label 1", "Supp Label 2", "Supp Label 3", "Supp Label 1", "Supp Label 2", "Supp Label 3", "Supp Label 1", "Supp Label 2", "Supp Label 3"),
#'   QORIG = c("CRF", "CRF", "CRF", "CRF", "CRF", "CRF", "CRF", "CRF", "CRF"),
#'   QEVAL = c("", "", "", "", "", "", "", "", "")
#' )
#'
#' library(tidyverse)
#' suppdm <- make_supp_df(dm, suppmeta)
#' suppae <- make_supp_df(ae, suppmeta)
#' suppcm <- make_supp_df(apcm, suppmeta)
make_supp_df <- function(df, suppmeta, idver = NULL) {
  # Check parameters df: dataframe, suppmeta: dataframe, domain: string
  if (!is.data.frame(df)) {
    stop("df must be a dataframe")
  }
  if (!is.data.frame(suppmeta)) {
    stop("suppmeta must be a dataframe")
  }

  # Check if suppmeta has the right columns: RDOMAIN, IDVAR, QNAM, QLABEL, QORIG, QEVAL
  suppmeta_vars <- c("RDOMAIN", "IDVAR", "QNAM", "QLABEL", "QORIG", "QEVAL")
  if (!all(suppmeta_vars %in% colnames(suppmeta))) {
    stop(paste("suppmeta must have columns ", suppmeta_vars, "\n", sep = ":"))
  }

  # Get a domain from df, if the domain is not unique, return an error message
  domain <- unique(df$DOMAIN)
  if (length(domain) != 1) {
    stop("DOMAIN must be unique")
  }

  # Get a QNAM(SUPP variable) vector from suppmeta when RDOMAIN is domain
  suppmeta <- suppmeta[suppmeta$RDOMAIN == domain, ]
  if(!is.null(idver)) {
    suppmeta$IDVAR <- idver
  }

  act_supp_qnam <- intersect(colnames(df), suppmeta$QNAM)

  #  Pivot the df to long format and join with suppmeta
  suppqual_vars <- c("STUDYID", "RDOMAIN", "USUBJID", "IDVAR", "IDVARVAL", "QNAM", "QLABEL", "QVAL", "QORIG", "QEVAL")
  if(str_sub(domain, 1, 2) == "AP") {
    suppqual_vars[suppqual_vars == "USUBJID"] <- "APID"
  }
  tryCatch({
    df_long <- df |>
      pivot_longer(cols = act_supp_qnam, names_to = "QNAM", values_to = "QVAL") |>
      filter(!is.na(QVAL) & QVAL != "") |>
      left_join(suppmeta, by = c("DOMAIN" = "RDOMAIN", "QNAM" = "QNAM")) |>
      rowwise() |>
      mutate(
        IDVARVAL = as.character(ifelse(IDVAR == "" | is.na(IDVAR), "", get(IDVAR))),
        RDOMAIN = DOMAIN
        ) |>
      ungroup() |>
      select(all_of(suppqual_vars))
  }, error = function(e) {
    stop(paste0("ERROR: Making a supp: ", e$message))
  })

  df_long
}


#' Split the QVAL column in a supplementary data frame
#'
#' This function splits the QVAL column in a supplementary data frame into multiple rows, ensuring that each split part does not exceed 200 bytes size.
#'
#' @param supp_df A tibble containing the supplementary data frame with a QVAL column.
#' @return A tibble with the QVAL column split into multiple rows.
#' @examples
#' library(dplyr)
#' library(tidyr)
#' library(purrr)
#' supp_df <- tibble(
#'   ID = 1:3,
#'   QVAL = c("This is a very long text that needs to be split into smaller parts. This is a very long text that needs to be split into smaller parts. This is a very long text that needs to be split into smaller parts. This is a very long text that needs to be split into smaller parts.",
#'            "Another long text that needs splitting.",
#'            "Short text"),
#'   QNAM = c("Q1", "Q2", "Q3")
#' )
#' result <- split_supp_qval(supp_df)
#' print(result)
#' @importFrom dplyr mutate rowwise
#' @importFrom tidyr unnest
#' @importFrom purrr map
#' @export
split_supp_qval <- function(supp_df) {
  ## Check if supp_df is a tibble
  if (!is_tibble(supp_df)) {
    stop("supp_df must be a tibble")
  }

  ## Check if QVAL is in supp_df
  if (!"QVAL" %in% colnames(supp_df)) {
    stop("QVAL must be in supp_df")
  }

  sep_texts <- map(supp_df$QVAL, ~ split_text_by_max_bytes(., max_bytes = 200))

  ## Loop through each row of supp_df
  supp_df_split <- supp_df %>%
    mutate(QVAL = sep_texts) %>%
    rowwise() %>%
    mutate(QNAM = list(get_split_var_names(QNAM, length(QVAL)))) %>%
    unnest(cols = c(QNAM, QVAL))

  return(supp_df_split)
}
