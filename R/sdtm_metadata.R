#' Read SDTM Metadata from P21 Spec File
#'
#' @description
#' This function reads SDTM metadata from a P21 Excel file, extracting dataset and variable metadata.
#'
#' @param spec A character string specifying the path to the P21 Excel file.
#'
#' @return A list containing two data frames: `datasets` and `variables`, which hold the metadata for datasets and variables respectively.
#'
#' @import readxl
#' @import dplyr
#' @export
read_sdtm_metadata_p21 <- function(spec) {
  # Confirm the file path and the file type of spec
  if (!file.exists(spec)) {
    stop("The file does not exist.")
  } else if (!grepl("\\.(xls|xlsx)$", spec)) {
    stop("The file is not an Excel file.")
  }

  sdtm_meta <- list()

  tryCatch({
    # Get sheet names
    sheets <- readxl::excel_sheets(spec)

    # Get sheet names for a domain/variable/supp sheet
    domain_sheet <- sheets[grep("^dataset$|^datasets$", sheets, ignore.case = TRUE)]
    variable_sheet <- sheets[grep("^variable$|^variables$", sheets, ignore.case = TRUE)]

    # Read datasets metadata
    sdtm_meta$datasets <- read_meta_datasets_p21(spec, domain_sheet[1])

    # Read variables metadata
    sdtm_meta$variables <- read_meta_variables_p21(spec, variable_sheet[1])
  }, error = function(e) {
    stop("Error in processing Excel file: ", e$message)
  })

  return(sdtm_meta)
}


read_meta_datasets_p21 <- function(spec, domain_sheet) {
  # Excelファイルを読み込む
  meta_domain <- readxl::read_excel(spec, sheet = domain_sheet)

  # 列名を大文字小文字を区別しないように変換
  colnames(meta_domain) <- tolower(colnames(meta_domain))

  # datasetとlabelの列を探す
  dataset_col <- grep("^dataset$", colnames(meta_domain), ignore.case = TRUE, value = TRUE)
  label_col <- grep("^label$", colnames(meta_domain), ignore.case = TRUE, value = TRUE)
  key_col <- grep("^key variables$", colnames(meta_domain), ignore.case = TRUE, value = TRUE)

  # 完全一致がない場合、domainまたはlabelから始まる列名を探し、最初の列を採用する
  if (length(dataset_col) == 0) {
    dataset_col <- grep("^domain", colnames(meta_domain), ignore.case = TRUE, value = TRUE)
    if (length(dataset_col) > 0) {
      dataset_col <- dataset_col[1]
    }
  }
  if (length(label_col) == 0) {
    label_col <- grep("^label", colnames(meta_domain), ignore.case = TRUE, value = TRUE)
    if (length(label_col) > 0) {
      label_col <- label_col[1]
    }
  }
  if (length(key_col) == 0) {
    key_col <- grep("^key", colnames(meta_domain), ignore.case = TRUE, value = TRUE)
    if (length(key_col) > 0) {
      key_col <- key_col[1]
    }
  }

  # 必要な列を選択
  meta_domain |>
    select(dataset_col[1], label_col[1], key_col[1]) |>
    rename(dataset = dataset_col[1], label = label_col[1], key = key_col[1])
}

read_meta_variables_p21 <- function(spec, variables_sheet) {
  # Excelファイルを読み込む
  meta_variables <- readxl::read_excel(spec, sheet = variables_sheet)

  # 列名を小文字に変換
  colnames(meta_variables) <- tolower(colnames(meta_variables))

  # 必要な列を探す
  dataset_col <- grep("^dataset$", colnames(meta_variables), value = TRUE)
  variable_col <- grep("^variable$", colnames(meta_variables), value = TRUE)
  label_col <- grep("^label$", colnames(meta_variables), value = TRUE)
  type_col <- grep("^data type$", colnames(meta_variables), value = TRUE)
  order_col <- grep("^order$", colnames(meta_variables), value = TRUE)

  # 完全一致がない場合、各列名から始まる列名を探し、最初の列を採用する
  if (length(dataset_col) == 0) {
    dataset_col <- grep("^dataset", colnames(meta_variables), value = TRUE)
    if (length(dataset_col) > 0) {
      dataset_col <- dataset_col[1]
    }
  }
  if (length(variable_col) == 0) {
    variable_col <- grep("^variable", colnames(meta_variables), value = TRUE)
    if (length(variable_col) > 0) {
      variable_col <- variable_col[1]
    }
  }
  if (length(label_col) == 0) {
    label_col <- grep("^label", colnames(meta_variables), value = TRUE)
    if (length(label_col) > 0) {
      label_col <- label_col[1]
    }
  }
  if (length(type_col) == 0) {
    type_col <- grep("^type", colnames(meta_variables), value = TRUE)
    if (length(type_col) > 0) {
      type_col <- type_col[1]
    }
  }
  if (length(order_col) == 0) {
    order_col <- grep("^order", colnames(meta_variables), value = TRUE)
    if (length(order_col) > 0) {
      order_col <- order_col[1]
    }
  }

  # 必要な列を選択
  meta_variables <- meta_variables |>
    select(dataset_col[1], variable_col[1], label_col[1], type_col[1], order_col[1]) |>
    rename(dataset = dataset_col[1], variable = variable_col[1], label = label_col[1], type = type_col[1], order = order_col[1])
  # 結果を返す
  return(meta_variables)
}

#' Read SDTM Supplemental Qualifiers Metadata from Excel File
#'
#' @description
#' This function reads SDTM supplemental qualifiers metadata from a specified sheet in an Excel file.
#'
#' @param spec A character string specifying the path to the Excel file.
#' @param supp_sheet A character string specifying the name of the sheet containing the supplemental qualifiers metadata. Default is "Suppqual".
#' @param dataset_col A character string specifying the name of the column containing dataset names. Default is "Dataset".
#' @param qnam_col A character string specifying the name of the column containing QNAM values. Default is "QNAM".
#' @param qlabel_col A character string specifying the name of the column containing QLABEL values. Default is "QLABEL".
#' @param qevel_col A character string specifying the name of the column containing QEVAL values. Default is "QEVAL".
#' @param qorig_col A character string specifying the name of the column containing QORIG values. Default is "QORIG".
#' @param idvar_col A character string specifying the name of the column containing IDVAR values. Default is "IDVAR".
#'
#' @return A data frame containing the supplemental qualifiers metadata.
#'
#' @import readxl
#' @import dplyr
#' @export
read_sdtm_meta_supp <- function(spec, supp_sheet = "Suppqual", dataset_col = "Dataset",
                                qnam_col = "QNAM", qlabel_col = "QLABEL", qevel_col = "QEVAL",
                                qorig_col = "QORIG", idvar_col = "IDVAR") {
  # Confirm the file path and the file type of spec
  if (!file.exists(spec)) {
    stop("The file does not exist.")
  } else if (!grepl("\\.(xls|xlsx)$", spec)) {
    stop("The file is not an Excel file.")
  }

  if (is.null(idvar_col) || is.na(idvar_col)) {
    idvar_col = ""
  }

  tryCatch({
    # Get sheet names
    sheets <- readxl::excel_sheets(spec)

    # Find the sheet name that matches supp_sheet (case insensitive)
    matched_sheet <- sheets[tolower(sheets) == tolower(supp_sheet)]
    if (length(matched_sheet) == 0) {
      stop("The specified sheet does not exist in the Excel file.")
    }


    # Excelファイルを読み込む
    meta_supp <- readxl::read_excel(spec, sheet = matched_sheet[1])

    # dataset列をRDOMAINに変換
    if (!is.null(dataset_col)) {
      meta_supp <- meta_supp |>
        mutate(RDOMAIN = toupper(!!sym(dataset_col))) |>
        mutate(RDOMAIN = sub("^supp|^sq", "", RDOMAIN, ignore.case = TRUE)) |>
        mutate(RDOMAIN = case_when(
          grepl("^AP", RDOMAIN) ~ substr(RDOMAIN, 1, 4),
          TRUE ~ substr(RDOMAIN, 1, 2)
        ))
    }

    # 必要な列を選択
    meta_supp <- meta_supp %>%
      mutate(
        RDOMAIN = RDOMAIN,
        QNAM    = !!sym(qnam_col),
        QLABEL  = !!sym(qlabel_col),
        QEVAL   = !!sym(qevel_col),
        QORIG   = !!sym(qorig_col),
        IDVAR   = if (idvar_col %in% colnames(.)) !!sym(idvar_col) else ""
      ) %>%
      filter(!is.na(RDOMAIN) & RDOMAIN != "") %>%
      select(RDOMAIN, QNAM, QLABEL, QEVAL, QORIG, IDVAR)

  }, error = function(e) {
    stop("Error in processing Excel file: ", e$message)
  })

  # 結果を返す
  return(meta_supp)
}
