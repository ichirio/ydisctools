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
#' cm <- data.frame(
#'   STUDYID = c("HOGE-01", "HOGE-01", "HOGE-01"),
#'   DOMAIN = c("CM", "CM", "CM"),
#'   USUBJID = c("HOGE-01-001", "HOGE-01-002", "HOGE-01-003"),
#'   CMSPID = c("CM-001", "CM-002", "CM-003"),
#'   CMTRT = c("trt1", "trt2", "trt3"),
#'   SUPP1 = c("supp1a", "supp1b", "supp1c"),
#'   SUPP2 = c("supp2a", "supp2b", ""),
#'   SUPP3 = c("", "", "")
#' )
#'
#' suppmeta <- data.frame(
#'   RDOMAIN = c("DM", "DM", "DM", "AE", "AE", "AE", "CM", "CM", "CM"),
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
#' suppcm <- make_supp_df(cm, suppmeta)
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
#  supp_vars <- suppmeta$QNAM[suppmeta$RDOMAIN == domain]


  #  Pivot the df to long format and join with suppmeta
  suppqual_vars <- c("STUDYID", "RDOMAIN", "USUBJID", "IDVAR", "IDVARVAL", "QNAM", "QLABEL", "QVAL", "QORIG", "QEVAL")
  tryCatch({
    df_long <- df |>
      pivot_longer(cols = suppmeta$QNAM, names_to = "QNAM", values_to = "QVAL") |>
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
