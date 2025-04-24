#' Read Multiple SAS Datasets from a Folder
#'
#' This function reads multiple SAS datasets (sas7bdat format) from a specified folder.
#' Optionally, specific datasets can be selected by providing their names.
#'
#' @param folder A character string specifying the path to the folder containing the SAS datasets.
#' @param datasets A character vector of dataset names (without file extensions) to be read.
#' If `NA`, all datasets in the folder with the `.sas7bdat` extension are read. Default is `NA`.
#'
#' @return A named list of data frames, where each data frame corresponds to a SAS dataset.
#' The names of the list are derived from the dataset filenames (without the `.sas7bdat` extension).
#'
#' @export
#' @import haven purrr
#'
#' @examples
#' \dontrun{
#' # Read all SAS datasets in the folder
#' datasets <- read_sas_datasets("path/to/folder")
#'
#' # Read specific datasets
#' datasets <- read_sas_datasets("path/to/folder", datasets = c("dataset1", "dataset2"))
#' }
read_sas_datasets <- function(folder, datasets=NA, tz="") {
  sas_files <- dir(pattern = "\\.(?i)sas7bdat$", folder)
  if(!all(is.na(datasets))) {
    sas_files <- sas_files[toupper(sas_files) %in% toupper(paste0(datasets, ".sas7bdat"))]
  }

  result <- sas_files %>%
    map(~{
      read_sas(file.path(folder, .)) |>
        mutate(across(where(is.POSIXct), ~as.POSIXct(as.POSIXlt(., tz = "UTC"), tz = tz)))
  })

  names(result) <- sub("\\.(?i)sas7bdat$", "", sas_files)

  result
}

#' Read XPT Files from a Folder
#'
#' This function reads XPT files from a specified folder and returns them as a list of data frames. Optionally, a subset of XPT files can be specified.
#'
#' @param folder A character string specifying the path to the folder containing the XPT files.
#' @param xpts A character vector specifying the names of the XPT files to read (without the .xpt extension). If NA, all XPT files in the folder are read. Default is NA.
#' @return A named list of data frames, where each data frame corresponds to an XPT file.
#' @examples
#' \dontrun{
#' # Read all XPT files from a folder
#' data_list <- read_xpts("path/to/folder")
#'
#' # Read specific XPT files from a folder
#' data_list <- read_xpts("path/to/folder", xpts = c("file1", "file2"))
#' }
#' @import purrr
#' @import haven
#' @export
read_xpts <- function(folder, xpts=NA, tz="") {
  sas_files <- dir(pattern = "\\.(?i)xpt$", folder)
  if(!all(is.na(xpts))) {
    sas_files <- sas_files[toupper(sas_files) %in% toupper(paste0(xpts, ".xpt"))]
  }

  result <- sas_files %>%
    map(~{
      read_xpt(file.path(folder, .)) |>
        mutate(across(where(is.POSIXct), ~as.POSIXct(as.POSIXlt(., tz = "UTC"), tz = tz)))
    })

  names(result) <- sub("\\.(?i)xpt$", "", sas_files)

  result
}

#' Filter Subjects from a List of Data Frames
#'
#' This function filters subjects (rows) from a list of data frames based on a specified variable and its value(s).
#' If the variable is present in the data frame, the rows where the variable matches the specified values are retained.
#' If no values are provided for filtering, the function returns the original list of data frames.
#'
#' @param df_list A list of data frames to be filtered.
#' @param varname A character string specifying the name of the variable (column) to filter by.
#' @param varval A vector of values used to filter the rows of the data frames. If `NA`, no filtering is applied. Default is `NA`.
#'
#' @return A list of filtered data frames. If a data frame does not contain the specified variable, it is returned unmodified.
#'
#' @export
#' @import purrr dplyr
#'
#' @examples
#' \dontrun{
#' # Example with filtering based on a variable
#' filtered_dfs <- pickup_subjects(df_list, "subject_id", c("101", "102"))
#'
#' # Example without filtering (returns the original list)
#' all_dfs <- pickup_subjects(df_list, "subject_id")
#' }
pickup_subjects <- function(df_list, varname, varval = NA) {
  if(!all(is.na(varval))) {
    df_list <- df_list %>%
      map(~
          if(any(names(.) %in% varname)) {
              dplyr::filter(., .[[varname]] %in% varval)
            }
          else {
            .
          })
  }
  df_list
}
