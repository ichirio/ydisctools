#' Create SDTM + SUPP variables in a dataset with metadata
#'
#' This function creates an SDTM (Study Data Tabulation Model) dataset for a specified domain, including supplemental variables if available.
#'
#' @param df A data frame containing the raw data.
#' @param domain A character string specifying the domain name.
#' @param sdtm_meta A list containing SDTM metadata, including datasets, variables, and supplemental qualifiers.
#' @param xpt_path An optional character string specifying the path to save the dataset as an XPT file. Default is NULL.
#' @return A data frame representing the SDTM dataset for the specified domain.
#' @import dplyr
#' @importFrom xportr xportr_df_label xportr_order xportr_type xportr_label
#' @importFrom haven write_xpt
#' @export
#' @examples
#' # Example usage:
#' df <- data.frame(ID = 1:3, VAR1 = c("A", "B", "C"), VAR2 = c(10, 20, 30))
#' sdtm_meta <- list(
#'   datasets = data.frame(dataset = "DM", label = "Demographics"),
#'   variables = data.frame(dataset = "DM", variable = c("ID", "VAR1", "VAR2"), label = c("Identifier", "Variable 1", "Variable 2")),
#'   suppqual = data.frame(RDOMAIN = "DM", QNAM = "VAR3", QLABEL = "Supplemental Variable")
#' )
#' make_sdtm_all_dataset(df, "DM", sdtm_meta)
make_sdtm_all_dataset <- function(df, domain, sdtm_meta, xpt_path = NULL) {
  target_domain <- toupper(domain)
  target_file <- paste0(tolower(target_domain), "_all.xpt")

  datasets_meta <- sdtm_meta[[grep("^dataset$|^datasets$", names(sdtm_meta), ignore.case = TRUE)]]
  variables_meta <- sdtm_meta[[grep("^variable$|^variables$", names(sdtm_meta), ignore.case = TRUE)]]
  supp_meta <- sdtm_meta[[grep("^suppqual$|^supp$", names(sdtm_meta), ignore.case = TRUE)]]

  domain_vars <- na.omit(variables_meta$variable[variables_meta$dataset == target_domain])
  supp_vars <- na.omit(supp_meta$QNAM[supp_meta$RDOMAIN == target_domain])

  variables_meta <- supp_meta |>
    filter(RDOMAIN == target_domain) |>
    mutate(type = "text", order = NA) |>
    select(dataset = RDOMAIN, variable = QNAM, label = QLABEL, type, order) |>
    bind_rows(variables_meta)

  target <- df |>
    select(all_of(domain_vars), any_of(supp_vars)) |>
    xportr_df_label(metadata = datasets_meta, domain = target_domain) |>
    xportr_order(metadata = variables_meta, domain = target_domain) |>
    xportr_type(metadata = variables_meta, domain = target_domain) |>
    xportr_label(metadata = variables_meta, domain = target_domain)

  for(i in names(target)) {
    attr(target[[i]], "format.sas") <- NULL
  }

  if(!is.null(xpt_path)) {
    target |>
      write.xport(file.path(xpt_path, target_file), version = 5, name = target_domain)
  }

  return(target)
}

#' Create SDTM Dataset with metadata
#'
#' This function creates an SDTM (Study Data Tabulation Model) dataset for a specified domain.
#'
#' @param df A data frame containing the raw data.
#' @param domain A character string specifying the domain name.
#' @param sdtm_meta A list containing SDTM metadata, including datasets and variables.
#' @param xpt_path An optional character string specifying the path to save the dataset as an XPT file. Default is NULL.
#' @return A data frame representing the SDTM dataset for the specified domain.
#' @import dplyr
#' @importFrom xportr xportr_df_label xportr_order xportr_type xportr_label
#' @importFrom haven write_xpt
#' @export
#' @examples
#' # Example usage:
#' df <- data.frame(ID = 1:3, VAR1 = c("A", "B", "C"), VAR2 = c(10, 20, 30))
#' sdtm_meta <- list(
#'   datasets = data.frame(dataset = "DM", label = "Demographics"),
#'   variables = data.frame(dataset = "DM", variable = c("ID", "VAR1", "VAR2"), label = c("Identifier", "Variable 1", "Variable 2"))
#' )
#' make_sdtm_dataset(df, "DM", sdtm_meta)
make_sdtm_dataset <- function(df, domain, sdtm_meta, xpt_path = NULL) {
  target_domain <- toupper(domain)
  target_file <- paste0(tolower(target_domain), ".xpt")

  datasets_meta <- sdtm_meta[[grep("^dataset$|^datasets$", names(sdtm_meta), ignore.case = TRUE)]]
  variables_meta <- sdtm_meta[[grep("^variable$|^variables$", names(sdtm_meta), ignore.case = TRUE)]]

  domain_vars <- na.omit(variables_meta$variable[variables_meta$dataset == target_domain])


  target <- df |>
    select(all_of(domain_vars)) |>
    xportr_df_label(metadata = datasets_meta, domain = target_domain) |>
    xportr_order(metadata = variables_meta, domain = target_domain) |>
    xportr_type(metadata = variables_meta, domain = target_domain) |>
    xportr_label(metadata = variables_meta, domain = target_domain)

  for(i in names(target)) {
    attr(target[[i]], "format.sas") <- NULL
  }

  if(!is.null(xpt_path)) {
    target |>
      write.xport(file.path(xpt_path, target_file), version = 5, name = target_domain)
  }

  return(target)
}

#' Create SDTM Supplemental Dataset with metadata
#'
#' This function creates an SDTM (Study Data Tabulation Model) supplemental dataset for a specified domain.
#'
#' @param df A data frame containing the raw data.
#' @param domain A character string specifying the domain name.
#' @param sdtm_meta A list containing SDTM metadata, including datasets, variables, and supplemental qualifiers.
#' @param xpt_path An optional character string specifying the path to save the dataset as an XPT file. Default is NULL.
#' @return A data frame representing the SDTM supplemental dataset for the specified domain.
#' @import dplyr
#' @importFrom ydisctools make_supp_df
#' @importFrom xportr xportr_df_label xportr_order xportr_type xportr_label
#' @importFrom haven write_xpt
#' @export
#' @examples
#' # Example usage:
#' df <- data.frame(ID = 1:3, VAR1 = c("A", "B", "C"), VAR2 = c(10, 20, 30))
#' sdtm_meta <- list(
#'   datasets = data.frame(dataset = "DM", label = "Demographics"),
#'   variables = data.frame(dataset = "DM", variable = c("ID", "VAR1", "VAR2"), label = c("Identifier", "Variable 1", "Variable 2")),
#'   suppqual = data.frame(RDOMAIN = "DM", QNAM = "VAR3", QLABEL = "Supplemental Variable")
#' )
#' make_sdtm_supp_dataset(df, "DM", sdtm_meta)
make_sdtm_supp_dataset <- function(df, domain, sdtm_meta, xpt_path = NULL) {
  target_domain <- toupper(domain)
  target_file <- paste0(tolower(target_domain), ".xpt")

  datasets_meta <- sdtm_meta[[grep("^dataset$|^datasets$", names(sdtm_meta), ignore.case = TRUE)]]
  variables_meta <- sdtm_meta[[grep("^variable$|^variables$", names(sdtm_meta), ignore.case = TRUE)]]
  supp_meta <- sdtm_meta[[grep("^suppqual$|^supp$", names(sdtm_meta), ignore.case = TRUE)]]

  supp_vars <- na.omit(supp_meta$QNAM[supp_meta$RDOMAIN == gsub("SUPP|SQ", "", target_domain, ignore.case = TRUE)])

  target <- df |>
    make_supp_df(supp_meta) |>
    xportr_df_label(metadata = datasets_meta, domain = target_domain) |>
    xportr_order(metadata = variables_meta, domain = target_domain) |>
    xportr_type(metadata = variables_meta, domain = target_domain) |>
    xportr_label(metadata = variables_meta, domain = target_domain)

  for(i in names(target)) {
    attr(target[[i]], "format.sas") <- NULL
  }

  if(!is.null(xpt_path)) {
    target |>
      write.xport(file.path(xpt_path, target_file), version = 5, name = target_domain)
  }

  return(target)
}
