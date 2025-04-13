#' @name read_p21_spec
#' @title Read P21 Specification Files
#'
#' @description
#' These functions read metadata from P21 specification Excel files. They handle different
#' types of metadata including datasets, variables, codelists, value levels, dictionaries,
#' and methods.
#'
#' @param spec A character string specifying the path to the P21 Excel file.
#' @param domain_sheet A character string specifying the name of the sheet containing dataset metadata.
#' @param variables_sheet A character string specifying the name of the sheet containing variable metadata.
#' @param codelists_sheet A character string specifying the name of the sheet containing codelist metadata.
#' @param valuelevel_sheet A character string specifying the name of the sheet containing value level metadata.
#' @param dictionaries_sheet A character string specifying the name of the sheet containing dictionary metadata.
#' @param methods_sheet A character string specifying the name of the sheet containing method metadata.
#'
#' @return A data frame containing the specified metadata.
#'
#' @import readxl
#' @import dplyr
#' @export
read_p21_spec <- function(spec) {
  # Confirm the file path and the file type of spec
  if (!file.exists(spec)) {
    stop("The file does not exist.")
  } else if (!grepl("\\.(xls|xlsx)$", spec)) {
    stop("The file is not an Excel file.")
  }

  p21_meta <- list()

  tryCatch({
    # Get sheet names
    sheets <- readxl::excel_sheets(spec)

    # Get sheet names for each type
    domain_sheet <- sheets[grep("^dataset$|^datasets$", sheets, ignore.case = TRUE)]
    variable_sheet <- sheets[grep("^variable$|^variables$", sheets, ignore.case = TRUE)]
    codelists_sheet <- sheets[grep("^codelist$|^codelists$", sheets, ignore.case = TRUE)]
    valuelevels_sheet <- sheets[grep("^valuelevel$|^valuelevels$", sheets, ignore.case = TRUE)]
    dictionaries_sheet <- sheets[grep("^dictionary$|^dictionaries$", sheets, ignore.case = TRUE)]
    methods_sheet <- sheets[grep("^method$|^methods$", sheets, ignore.case = TRUE)]
    comments_sheet <- sheets[grep("^comment$|^comments$", sheets, ignore.case = TRUE)]
    documents_sheet <- sheets[grep("^document$|^documents$", sheets, ignore.case = TRUE)]
    define_sheet <- sheets[grep("^define$", sheets, ignore.case = TRUE)]

    # Check if all required sheets exist
    required_sheets <- c("Methods", "Comments")
    missing_sheets <- required_sheets[!tolower(required_sheets) %in% tolower(sheets)]
    if (length(missing_sheets) > 0) {
      stop("This file does not appear to be a P21 specification file. Missing required sheets: ",
           paste(missing_sheets, collapse = ", "))
    }

    # Read metadata from each sheet
    result <- list()
    result$datasets <- read_p21_spec_datasets(spec, domain_sheet[1])
    result$variables <- read_p21_spec_variables(spec, variable_sheet[1])
    result$codelists <- read_p21_spec_codelists(spec, codelists_sheet[1])
    result$valuelevels <- read_p21_spec_valuelevels(spec, valuelevels_sheet[1])
    result$dictionaries <- read_p21_spec_dictionaries(spec, dictionaries_sheet[1])
    result$methods <- read_p21_spec_methods(spec, methods_sheet[1])
    result$comments <- read_p21_spec_comments(spec, comments_sheet[1])
    result$documents <- read_p21_spec_documents(spec, documents_sheet[1])
    result$define <- read_p21_spec_define(spec, define_sheet[1])

    # Add ORIGINAL list containing all sheets without any modification
    result$ORIGINAL <- read_excel_all_sheets(spec)
  }, error = function(e) {
    stop("Error in processing Excel file: ", e$message)
  })

  return(result)
}

#' @rdname read_p21_spec
#' @export
read_p21_spec_datasets <- function(spec, domain_sheet) {
  # Excelファイルを読み込む
  meta_domain <- readxl::read_excel(spec, sheet = domain_sheet)
  colnames(meta_domain) <- tolower(colnames(meta_domain))

  # 列名のパターンを定義
  col_patterns <- list(
    dataset = "^dataset$|^datasets$|^domain$",
    label = "^label$|^labels$|^description$",
    class = "^class$|^classes$",
    subclass = "^subclass$|^subclasses$",
    structure = "^structure$|^structures$",
    key = "^key variables$|^key variable$|^key$",
    standard = "^standard$|^standards$",
    has_no_data = "^has no data$|^no data$",
    repeating = "^repeating$|^repeat$",
    reference_data = "^reference data$|^reference$",
    comment = "^comment$|^comments$",
    developer_notes = "^developer notes$|^developer note$|^dev notes$"
  )

  # 列を選択
  selected_cols <- lapply(col_patterns, function(pattern) {
    cols <- grep(pattern, colnames(meta_domain), ignore.case = TRUE, value = TRUE)
    if (length(cols) > 0) cols[1] else NULL
  })
  selected_cols <- selected_cols[!sapply(selected_cols, is.null)]

  # その他の列を追加（.1のような列名は除外）
  other_cols <- setdiff(colnames(meta_domain), unlist(selected_cols))
  other_cols <- other_cols[!grepl("^\\.\\d+$", other_cols)]

  # 結果のデータフレームを作成
  result <- meta_domain |>
    select(all_of(unlist(selected_cols)), all_of(other_cols)) |>
    rename_with(~ tolower(gsub("\\s+", "_", .)), everything())

  return(result)
}

#' @rdname read_p21_spec
#' @export
read_p21_spec_variables <- function(spec, variables_sheet) {
  # Excelファイルを読み込む
  meta_variables <- readxl::read_excel(spec, sheet = variables_sheet)
  colnames(meta_variables) <- tolower(colnames(meta_variables))

  # 列名のパターンを定義
  col_patterns <- list(
    order = "^order$|^sequence$",
    dataset = "^dataset$|^datasets$|^domain$",
    variable = "^variable$|^variables$",
    label = "^label$|^labels$",
    type = "^data type$|^type$|^datatype$",
    length = "^length$|^field length$",
    sig_digits = "^significant digits$|^sig digits$|^decimal places$",
    format = "^format$|^display format$",
    mandatory = "^mandatory$|^required$",
    assigned_value = "^assigned value$|^default value$",
    codelist = "^codelist$|^code list$|^codes$",
    common = "^common$|^standard$",
    origin = "^origin$|^source$",
    source = "^source$|^derivation$",
    pages = "^pages$|^page$",
    method = "^method$|^algorithm$",
    predecessor = "^predecessor$|^previous$",
    role = "^role$|^variable role$",
    has_no_data = "^has no data$|^no data$",
    comment = "^comment$|^comments$",
    developer_notes = "^developer notes$|^developer note$|^dev notes$"
  )

  # 列を選択
  selected_cols <- lapply(col_patterns, function(pattern) {
    cols <- grep(pattern, colnames(meta_variables), ignore.case = TRUE, value = TRUE)
    if (length(cols) > 0) cols[1] else NULL
  })
  selected_cols <- selected_cols[!sapply(selected_cols, is.null)]

  # その他の列を追加（.1のような列名は除外）
  other_cols <- setdiff(colnames(meta_variables), unlist(selected_cols))
  other_cols <- other_cols[!grepl("^\\.\\d+$", other_cols)]

  # 結果のデータフレームを作成
  result <- meta_variables |>
    select(all_of(unlist(selected_cols)), all_of(other_cols)) |>
    rename_with(~ tolower(gsub("\\s+", "_", .)), everything())

  return(result)
}

#' @rdname read_p21_spec
#' @export
read_p21_spec_codelists <- function(spec, codelists_sheet) {
  # Excelファイルを読み込む
  meta_codelists <- readxl::read_excel(spec, sheet = codelists_sheet)
  colnames(meta_codelists) <- tolower(colnames(meta_codelists))

  # 列名のパターンを定義
  col_patterns <- list(
    id = "^id$|^codelist id$",
    name = "^name$|^codelist name$",
    nci_codelist_code = "^nci codelist code$|^nci code$",
    type = "^data type$|^type$|^datatype$",
    terminology = "^terminology$|^term source$",
    comment = "^comment$|^comments$",
    order = "^order$|^sequence$",
    term = "^term$|^code$",
    nci_term_code = "^nci term code$|^nci code$",
    decode = "^decoded value$|^decode$|^description$"
  )

  # 列を選択
  selected_cols <- lapply(col_patterns, function(pattern) {
    cols <- grep(pattern, colnames(meta_codelists), ignore.case = TRUE, value = TRUE)
    if (length(cols) > 0) cols[1] else NULL
  })
  selected_cols <- selected_cols[!sapply(selected_cols, is.null)]

  # その他の列を追加（.1のような列名は除外）
  other_cols <- setdiff(colnames(meta_codelists), unlist(selected_cols))
  other_cols <- other_cols[!grepl("^\\.\\d+$", other_cols)]

  # 結果のデータフレームを作成
  result <- meta_codelists |>
    select(all_of(unlist(selected_cols)), all_of(other_cols)) |>
    rename_with(~ tolower(gsub("\\s+", "_", .)), everything())

  return(result)
}

#' @rdname read_p21_spec
#' @export
read_p21_spec_valuelevels <- function(spec, valuelevel_sheet) {
  # Excelファイルを読み込む
  meta_valuelevels <- readxl::read_excel(spec, sheet = valuelevel_sheet)
  colnames(meta_valuelevels) <- tolower(colnames(meta_valuelevels))

  # 列名のパターンを定義
  col_patterns <- list(
    order = "^order$|^sequence$",
    dataset = "^dataset$|^datasets$|^domain$",
    variable = "^variable$|^variables$",
    where_clause = "^where clause$|^where$|^condition$",
    label = "^label$|^labels$",
    type = "^data type$|^type$|^datatype$",
    length = "^length$|^field length$",
    sig_digits = "^significant digits$|^sig digits$|^decimal places$",
    format = "^format$|^display format$",
    mandatory = "^mandatory$|^required$",
    assigned_value = "^assigned value$|^default value$",
    codelist = "^codelist$|^code list$|^codes$",
    origin = "^origin$|^source$",
    source = "^source$|^derivation$",
    pages = "^pages$|^page$",
    method = "^method$|^algorithm$",
    predecessor = "^predecessor$|^previous$",
    comment = "^comment$|^comments$",
    developer_notes = "^developer notes$|^developer note$|^dev notes$"
  )

  # 列を選択
  selected_cols <- lapply(col_patterns, function(pattern) {
    cols <- grep(pattern, colnames(meta_valuelevels), ignore.case = TRUE, value = TRUE)
    if (length(cols) > 0) cols[1] else NULL
  })
  selected_cols <- selected_cols[!sapply(selected_cols, is.null)]

  # その他の列を追加（.1のような列名は除外）
  other_cols <- setdiff(colnames(meta_valuelevels), unlist(selected_cols))
  other_cols <- other_cols[!grepl("^\\.\\d+$", other_cols)]

  # 結果のデータフレームを作成
  result <- meta_valuelevels |>
    select(all_of(unlist(selected_cols)), all_of(other_cols)) |>
    rename_with(~ tolower(gsub("\\s+", "_", .)), everything())

  return(result)
}

#' @rdname read_p21_spec
#' @export
read_p21_spec_dictionaries <- function(spec, dictionaries_sheet) {
  # Excelファイルを読み込む
  meta_dictionaries <- readxl::read_excel(spec, sheet = dictionaries_sheet)
  colnames(meta_dictionaries) <- tolower(colnames(meta_dictionaries))

  # 列名のパターンを定義
  col_patterns <- list(
    id = "^id$|^dictionary id$",
    name = "^name$|^dictionary name$",
    type = "^data type$|^type$|^datatype$",
    dictionary = "^dictionary$|^dictionaries$|^source$",
    version = "^version$|^dictionary version$|^ver$",
    comment = "^comment$|^comments$",
    developer_notes = "^developer notes$|^developer note$|^dev notes$"
  )

  # 列を選択
  selected_cols <- lapply(col_patterns, function(pattern) {
    cols <- grep(pattern, colnames(meta_dictionaries), ignore.case = TRUE, value = TRUE)
    if (length(cols) > 0) cols[1] else NULL
  })
  selected_cols <- selected_cols[!sapply(selected_cols, is.null)]

  # その他の列を追加（.1のような列名は除外）
  other_cols <- setdiff(colnames(meta_dictionaries), unlist(selected_cols))
  other_cols <- other_cols[!grepl("^\\.\\d+$", other_cols)]

  # 結果のデータフレームを作成
  result <- meta_dictionaries |>
    select(all_of(unlist(selected_cols)), all_of(other_cols)) |>
    rename_with(~ tolower(gsub("\\s+", "_", .)), everything())

  return(result)
}

#' @rdname read_p21_spec
#' @export
read_p21_spec_methods <- function(spec, methods_sheet) {
  # Excelファイルを読み込む
  meta_methods <- readxl::read_excel(spec, sheet = methods_sheet)
  colnames(meta_methods) <- tolower(colnames(meta_methods))

  # 列名のパターンを定義
  col_patterns <- list(
    id = "^id$|^method id$",
    name = "^name$|^method name$",
    type = "^type$|^method type$",
    expression_context = "^expression context$|^context$",
    expression_code = "^expression code$|^code$|^expression$",
    comment = "^comment$|^comments$",
    developer_notes = "^developer notes$|^developer note$|^dev notes$"
  )

  # 列を選択
  selected_cols <- lapply(col_patterns, function(pattern) {
    cols <- grep(pattern, colnames(meta_methods), ignore.case = TRUE, value = TRUE)
    if (length(cols) > 0) cols[1] else NULL
  })
  selected_cols <- selected_cols[!sapply(selected_cols, is.null)]

  # その他の列を追加（.1のような列名は除外）
  other_cols <- setdiff(colnames(meta_methods), unlist(selected_cols))
  other_cols <- other_cols[!grepl("^\\.\\d+$", other_cols)]

  # 結果のデータフレームを作成
  result <- meta_methods |>
    select(all_of(unlist(selected_cols)), all_of(other_cols)) |>
    rename_with(~ tolower(gsub("\\s+", "_", .)), everything())

  return(result)
}

#' @rdname read_p21_spec
#' @export
read_p21_spec_comments <- function(spec, comments_sheet) {
  # Excelファイルを読み込む
  meta_comments <- readxl::read_excel(spec, sheet = comments_sheet)
  colnames(meta_comments) <- tolower(colnames(meta_comments))

  # 列名のパターンを定義
  col_patterns <- list(
    id = "^id$|^comment id$",
    dataset = "^dataset$|^datasets$|^domain$",
    variable = "^variable$|^variables$",
    type = "^type$|^comment type$",
    text = "^text$|^comment text$|^comment$",
    developer_notes = "^developer notes$|^developer note$|^dev notes$"
  )

  # 列を選択
  selected_cols <- lapply(col_patterns, function(pattern) {
    cols <- grep(pattern, colnames(meta_comments), ignore.case = TRUE, value = TRUE)
    if (length(cols) > 0) cols[1] else NULL
  })
  selected_cols <- selected_cols[!sapply(selected_cols, is.null)]

  # その他の列を追加（.1のような列名は除外）
  other_cols <- setdiff(colnames(meta_comments), unlist(selected_cols))
  other_cols <- other_cols[!grepl("^\\.\\d+$", other_cols)]

  # 結果のデータフレームを作成
  result <- meta_comments |>
    select(all_of(unlist(selected_cols)), all_of(other_cols)) |>
    rename_with(~ tolower(gsub("\\s+", "_", .)), everything())

  return(result)
}

read_p21_spec_documents <- function(spec, documents_sheet) {
  # Excelファイルを読み込む
  meta_documents <- readxl::read_excel(spec, sheet = documents_sheet)

  # 列名を小文字に変換
  colnames(meta_documents) <- tolower(colnames(meta_documents))

  # 必要な列を探す
  id_col <- grep("^id$", colnames(meta_documents), value = TRUE)
  title_col <- grep("^title$", colnames(meta_documents), value = TRUE)
  href_col <- grep("^href$", colnames(meta_documents), value = TRUE)

  # 完全一致がない場合、各列名から始まる列名を探し、最初の列を採用する
  if (length(id_col) == 0) {
    id_col <- grep("^id", colnames(meta_documents), value = TRUE)
    if (length(id_col) > 0) {
      id_col <- id_col[1]
    }
  }
  if (length(title_col) == 0) {
    title_col <- grep("^title", colnames(meta_documents), value = TRUE)
    if (length(title_col) > 0) {
      title_col <- title_col[1]
    }
  }
  if (length(href_col) == 0) {
    href_col <- grep("^href", colnames(meta_documents), value = TRUE)
    if (length(href_col) > 0) {
      href_col <- href_col[1]
    }
  }

  # 必要な列を選択
  meta_documents <- meta_documents |>
    select(id_col[1], title_col[1], href_col[1]) |>
    rename(id = id_col[1], title = title_col[1], href = href_col[1])

  # 結果を返す
  return(meta_documents)
}

read_p21_spec_define <- function(spec, define_sheet) {
  # Excelファイルを読み込む
  meta_define <- readxl::read_excel(spec, sheet = define_sheet)

  # 列名を小文字に変換
  colnames(meta_define) <- tolower(colnames(meta_define))

  # 必要な列を探す
  attribute_col <- grep("^attribute$", colnames(meta_define), value = TRUE)
  value_col <- grep("^value$", colnames(meta_define), value = TRUE)

  # 完全一致がない場合、各列名から始まる列名を探し、最初の列を採用する
  if (length(attribute_col) == 0) {
    attribute_col <- grep("^attribute", colnames(meta_define), value = TRUE)
    if (length(attribute_col) > 0) {
      attribute_col <- attribute_col[1]
    }
  }
  if (length(value_col) == 0) {
    value_col <- grep("^value", colnames(meta_define), value = TRUE)
    if (length(value_col) > 0) {
      value_col <- value_col[1]
    }
  }

  # 必要な列を選択
  meta_define <- meta_define |>
    select(attribute_col[1], value_col[1]) |>
    rename(attribute = attribute_col[1], value = value_col[1])

  # 空白行を除外（NA、空文字列、空白のみの行）
  meta_define <- meta_define |>
    filter(!is.na(attribute) & trimws(attribute) != "")

  # 期待される属性と値のデータフレームを作成
  expected_data <- data.frame(
    attribute = c("StudyName", "StudyDescription", "ProtocolName",
                 "StandardName", "StandardVersion", "Language"),
    value = c("CDISCPILOT01",
              "Study Data Tabulation Model Metadata Submission Guidelines Sample Study",
              "CDISCPILOT01", "", "", "en"),
    stringsAsFactors = FALSE
  )

  # 既存のデータと期待されるデータをマージ
  meta_define <- merge(expected_data, meta_define, by = "attribute", all.x = TRUE)

  # value.xとvalue.yを適切にマージ
  meta_define$value <- ifelse(is.na(meta_define$value.y),
                             meta_define$value.x,
                             meta_define$value.y)

  # 不要な列を削除
  meta_define <- meta_define |>
    select(attribute, value)

  # 期待される順序で並び替え
  meta_define <- meta_define |>
    arrange(factor(attribute, levels = expected_data$attribute))

  return(meta_define)
}

#' Read All Sheets from Excel File
#'
#' @description
#' This function reads all sheets from an Excel file without any modification to column names or data.
#'
#' @param spec A character string specifying the path to the Excel file.
#'
#' @return A named list containing all sheets from the Excel file. Each element is a data frame
#' representing a sheet, with the original column names preserved.
#'
#' @import readxl
#' @export
read_excel_all_sheets <- function(spec) {
  # Confirm the file path and the file type of spec
  if (!file.exists(spec)) {
    stop("The file does not exist.")
  } else if (!grepl("\\.(xls|xlsx)$", spec)) {
    stop("The file is not an Excel file.")
  }

  # Get all sheet names
  sheets <- readxl::excel_sheets(spec)

  # Read all sheets
  result <- list()
  for (sheet in sheets) {
    tryCatch({
      result[[sheet]] <- readxl::read_excel(spec, sheet = sheet)
    }, error = function(e) {
      warning(sprintf("Failed to read sheet '%s': %s", sheet, e$message))
      result[[sheet]] <- NULL
    })
  }

  return(result)
}
