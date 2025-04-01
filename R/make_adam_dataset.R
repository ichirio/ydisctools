#' Create an ADaM Dataset
#'
#' This function creates an ADaM (Analysis Data Model) dataset from a given input dataset (`df`) by applying metadata-driven transformations and optionally exporting it as an XPT file.
#'
#' @param df A data frame containing the input data to be transformed into an ADaM dataset.
#' @param domain A character string specifying the domain name (e.g., "ADSL", "ADAE").
#' @param adam_meta A list containing metadata for the ADaM dataset, including dataset-level and variable-level metadata.
#' @param xpt_path A character string specifying the directory path where the XPT file should be saved. Defaults to `NULL`, meaning no file will be written.
#' @param meta_length A logical value indicating whether to apply variable length metadata to the dataset. Defaults to `TRUE`.
#'
#' @return A transformed data frame representing the ADaM dataset, with metadata-driven labels, types, and ordering applied.
#'
#' @details
#' The function performs the following steps:
#' 1. Extracts domain-specific metadata for datasets and variables from the provided `adam_meta`.
#' 2. Selects and arranges variables based on the metadata.
#' 3. Applies metadata-driven transformations, including variable labels, types, and lengths.
#' 4. Optionally writes the resulting dataset to an XPT file in the specified directory.
#'
#' @examples
#' # Example usage:
#' df <- tibble::tibble(
#'   USUBJID = c("01", "02"),
#'   AGE = c(34, 45),
#'   SEX = c("M", "F")
#' )
#'
#' adam_meta <- list(
#'   datasets = tibble::tibble(
#'     dataset = c("ADSL"),
#'     key = c("USUBJID")
#'   ),
#'   variables = tibble::tibble(
#'     dataset = c("ADSL", "ADSL"),
#'     variable = c("USUBJID", "AGE"),
#'     label = c("Subject ID", "Age"),
#'     type = c("char", "num"),
#'     length = c(10, 3)
#'   )
#' )
#'
#' result <- make_adam_dataset(df, domain = "ADSL", adam_meta = adam_meta)
#'
#' @import dplyr
#' @importFrom xportr xportr_df_label xportr_order xportr_type xportr_label xportr_format xportr_length
#' @importFrom haven write_xpt
#' @export
make_adam_dataset <- function(df, domain, adam_meta, xpt_path = NULL, meta_length = TRUE) {
  target_domain <- toupper(domain)
  target_file <- paste0(tolower(target_domain), ".xpt")

  datasets_meta <- adam_meta[[grep("^dataset$|^datasets$", names(adam_meta), ignore.case = TRUE)]]
  variables_meta <- adam_meta[[grep("^variable$|^variables$", names(adam_meta), ignore.case = TRUE)]]

  domain_vars <- na.omit(variables_meta$variable[variables_meta$dataset == target_domain])
  key_vars <- datasets_meta$key[datasets_meta$dataset == target_domain]

  num_type <- getOption("xportr.numeric_metadata_type")
  chr_type <- getOption("xportr.character_metadata_type")
  options(xportr.numeric_metadata_type = c(num_type, "time"))
  options(xportr.character_metadata_type = chr_type[chr_type != "time"])

  target <- df |>
    select(all_of(domain_vars)) |>
    arrange(key_vars) |>
    xportr_df_label(metadata = datasets_meta, domain = target_domain) |>
    xportr_order(metadata = variables_meta, domain = target_domain) |>
    xportr_type(metadata = variables_meta, domain = target_domain) |>
    xportr_format(metadata = variables_meta, domain = target_domain) |>
    xportr_label(metadata = variables_meta, domain = target_domain)

  options(xportr.numeric_metadata_type = num_type)
  options(xportr.character_metadata_type = chr_type)

  if(meta_length) {
    target <- target |>
      xportr_length(metadata = variables_meta, domain = target_domain)
  }

  if(!is.null(xpt_path)) {
    target |>
      write_xpt(file.path(xpt_path, target_file), version = 5, name = target_domain)
  }

  return(target)
}
