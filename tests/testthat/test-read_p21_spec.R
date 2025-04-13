test_that("read_p21_spec works correctly", {
  spec_file <- test_path("test_spec.xls")
  
  # テストデータの読み込み
  result <- read_p21_spec(spec_file)
  
  # 基本的な構造のテスト
  expect_type(result, "list")
  expect_named(result, c("datasets", "variables", "codelists", "valuelevels", "dictionaries", 
                        "methods", "comments", "documents", "define", "ORIGINAL"))
  
  # 各コンポーネントの型確認
  expect_true(inherits(result$datasets, "data.frame"))
  expect_true(inherits(result$variables, "data.frame"))
  expect_true(inherits(result$codelists, "data.frame"))
  expect_true(inherits(result$valuelevels, "data.frame"))
  expect_true(inherits(result$dictionaries, "data.frame"))
  expect_true(inherits(result$methods, "data.frame"))
  expect_true(inherits(result$comments, "data.frame"))
  expect_true(inherits(result$documents, "data.frame"))
  expect_true(inherits(result$define, "data.frame"))
  expect_true(is.list(result$ORIGINAL))
  
  # データセットの行数確認
  expect_gt(nrow(result$datasets), 0)
  expect_gt(nrow(result$variables), 0)
  expect_gt(nrow(result$codelists), 0)
  expect_gt(nrow(result$valuelevels), 0)
  expect_gt(nrow(result$dictionaries), 0)
  expect_gt(nrow(result$methods), 0)
  expect_gt(nrow(result$comments), 0)
  expect_gt(nrow(result$documents), 0)
  expect_gt(nrow(result$define), 0)
  expect_gt(length(result$ORIGINAL), 0)
})

test_that("read_p21_spec handles errors correctly", {
  # 存在しないファイルのテスト
  expect_error(read_p21_spec("nonexistent.xls"), "The file does not exist")
  
  # 不正な拡張子のテスト
  temp_file <- tempfile(fileext = ".txt")
  file.create(temp_file)
  on.exit(unlink(temp_file))
  expect_error(read_p21_spec(temp_file), "The file is not an Excel file")
})

test_that("read_p21_spec_datasets works correctly", {
  spec_file <- test_path("test_spec.xls")
  
  # シート名を取得
  sheets <- readxl::excel_sheets(spec_file)
  domain_sheet <- sheets[grep("^dataset$|^datasets$", sheets, ignore.case = TRUE)][1]
  
  # テストデータの読み込み
  result <- read_p21_spec_datasets(spec_file, domain_sheet)
  
  # 基本的な構造のテスト
  expect_type(result, "list")
  expect_true(inherits(result, "data.frame"))
  
  # 必須列の存在確認
  expected_cols <- c("dataset", "label", "class", "subclass", "structure", 
                    "key", "standard", "has_no_data", "repeating", 
                    "reference_data", "comment", "developer_notes")
  for (col in expected_cols) {
    expect_true(any(grepl(col, names(result), ignore.case = TRUE)))
  }
  
  # データ型の確認
  expect_type(result$dataset, "character")
  expect_type(result$label, "character")
})

test_that("read_p21_spec_variables works correctly", {
  spec_file <- test_path("test_spec.xls")
  
  # シート名を取得
  sheets <- readxl::excel_sheets(spec_file)
  variable_sheet <- sheets[grep("^variable$|^variables$", sheets, ignore.case = TRUE)][1]
  
  # テストデータの読み込み
  result <- read_p21_spec_variables(spec_file, variable_sheet)
  
  # 基本的な構造のテスト
  expect_type(result, "list")
  expect_true(inherits(result, "data.frame"))
  
  # 必須列の存在確認
  expected_cols <- c("order", "dataset", "variable", "label", "type", 
                    "length", "sig_digits", "format", "mandatory", 
                    "assigned_value", "codelist", "common", "origin", 
                    "source", "pages", "method", "predecessor", "role", 
                    "has_no_data", "comment", "developer_notes")
  for (col in expected_cols) {
    expect_true(any(grepl(col, names(result), ignore.case = TRUE)))
  }
  
  # データ型の確認
  expect_type(result$dataset, "character")
  expect_type(result$variable, "character")
  expect_type(result$label, "character")
})

test_that("read_p21_spec_codelists works correctly", {
  spec_file <- test_path("test_spec.xls")
  
  # シート名を取得
  sheets <- readxl::excel_sheets(spec_file)
  codelist_sheet <- sheets[grep("^codelist$|^codelists$", sheets, ignore.case = TRUE)][1]
  
  # テストデータの読み込み
  result <- read_p21_spec_codelists(spec_file, codelist_sheet)
  
  # 基本的な構造のテスト
  expect_type(result, "list")
  expect_true(inherits(result, "data.frame"))
  
  # 必須列の存在確認
  expected_cols <- c("id", "name", "nci_codelist_code", "type", "terminology", 
                    "comment", "order", "term", "nci_term_code", "decode")
  for (col in expected_cols) {
    expect_true(any(grepl(col, names(result), ignore.case = TRUE)))
  }
  
  # データ型の確認
  expect_type(result$id, "character")
  expect_type(result$name, "character")
  expect_type(result$term, "character")
})

test_that("read_p21_spec_valuelevels returns correct data frame", {
  # テスト用のデータフレームを作成
  test_data <- data.frame(
    Order = 1:3,
    Dataset = c("DM", "DM", "AE"),
    Variable = c("SEX", "RACE", "AETERM"),
    `Where Clause` = c("SEX = 'M'", "RACE = 'ASIAN'", "AESEV = 'MILD'"),
    Label = c("Sex", "Race", "Adverse Event Term"),
    `Data Type` = c("Char", "Char", "Char"),
    Length = c(1, 10, 50),
    `Significant Digits` = c(NA, NA, NA),
    Format = c(NA, NA, NA),
    Mandatory = c("Yes", "Yes", "Yes"),
    `Assigned Value` = c(NA, NA, NA),
    Codelist = c("SEX", "RACE", NA),
    Origin = c("CRF", "CRF", "CRF"),
    Source = c(NA, NA, NA),
    Pages = c("DM-1", "DM-1", "AE-1"),
    Method = c(NA, NA, NA),
    Predecessor = c(NA, NA, NA),
    Comment = c(NA, NA, NA),
    `Developer Notes` = c(NA, NA, NA)
  )
  
  # 関数をモックして、テストデータを返すようにする
  mock_read_excel <- function(spec, sheet) {
    return(test_data)
  }
  
  # 元の関数を保存
  original_read_excel <- readxl::read_excel
  
  # モック関数に置き換え
  assignInNamespace("read_excel", mock_read_excel, ns = "readxl")
  
  # 関数を実行
  result <- read_p21_spec_valuelevels("dummy_file.xlsx", "ValueLevel")
  
  # 元の関数を復元
  assignInNamespace("read_excel", original_read_excel, ns = "readxl")
  
  # 結果の検証
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  
  # 列名の検証
  expected_cols <- c("order", "dataset", "variable", "where_clause", "label", 
                    "type", "length", "sig_digits", "format", "mandatory",
                    "assigned_value", "codelist", "origin", "source", "pages",
                    "method", "predecessor", "comment", "developer_notes")
  
  # 実際の列名を取得
  actual_cols <- tolower(names(result))
  
  # 主要な列が存在することを確認
  essential_cols <- c("order", "dataset", "variable", "label")
  for (col in essential_cols) {
    expect_true(any(grepl(col, actual_cols, fixed = TRUE)), 
                info = paste("Column", col, "not found in result"))
  }
  
  # データの検証
  expect_equal(result$dataset, c("DM", "DM", "AE"))
  expect_equal(result$variable, c("SEX", "RACE", "AETERM"))
  
  # where_clause列の検証（パターンマッチングを使用）
  where_clause_patterns <- c("^where_clause$", "^where$", "^condition$")
  where_clause_col <- grep(paste(where_clause_patterns, collapse = "|"), actual_cols, value = TRUE)
  if (length(where_clause_col) > 0) {
    expect_equal(result[[where_clause_col[1]]], c("SEX = 'M'", "RACE = 'ASIAN'", "AESEV = 'MILD'"))
  }
})

test_that("read_p21_spec_dictionaries returns correct data frame", {
  # テスト用のデータフレームを作成
  test_data <- data.frame(
    ID = c("DICT001", "DICT002", "DICT003"),
    Name = c("MedDRA", "WHO Drug", "SNOMED CT"),
    `Data Type` = c("Char", "Char", "Char"),
    Dictionary = c("MedDRA", "WHO Drug", "SNOMED CT"),
    Version = c("24.0", "2021", "2021-09-01"),
    Comment = c("Medical Dictionary", "Drug Dictionary", "Clinical Terms"),
    `Developer Notes` = c(NA, NA, NA)
  )
  
  # 関数をモックして、テストデータを返すようにする
  mock_read_excel <- function(spec, sheet) {
    return(test_data)
  }
  
  # 元の関数を保存
  original_read_excel <- readxl::read_excel
  
  # モック関数に置き換え
  assignInNamespace("read_excel", mock_read_excel, ns = "readxl")
  
  # 関数を実行
  result <- read_p21_spec_dictionaries("dummy_file.xlsx", "Dictionaries")
  
  # 元の関数を復元
  assignInNamespace("read_excel", original_read_excel, ns = "readxl")
  
  # 結果の検証
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  
  # 列名の検証
  expected_cols <- c("id", "name", "type", "dictionary", "version", "comment", "developer_notes")
  
  # 実際の列名を取得
  actual_cols <- tolower(names(result))
  
  # 主要な列が存在することを確認
  essential_cols <- c("id", "name", "type", "dictionary", "version")
  for (col in essential_cols) {
    expect_true(any(grepl(col, actual_cols, fixed = TRUE)), 
                info = paste("Column", col, "not found in result"))
  }
  
  # データの検証
  expect_equal(result$id, c("DICT001", "DICT002", "DICT003"))
  expect_equal(result$name, c("MedDRA", "WHO Drug", "SNOMED CT"))
  expect_equal(result$dictionary, c("MedDRA", "WHO Drug", "SNOMED CT"))
  expect_equal(result$version, c("24.0", "2021", "2021-09-01"))
})

test_that("read_p21_spec_methods returns correct data frame", {
  # テスト用のデータフレームを作成
  test_data <- data.frame(
    ID = c("METH001", "METH002", "METH003"),
    Name = c("Calculate Age", "Derive BMI", "Convert Units"),
    Type = c("Derivation", "Derivation", "Conversion"),
    Description = c("Calculate age in years", "Calculate BMI from height and weight", "Convert units to SI"),
    Context = c("DM", "VS", "LB"),
    Expression = c("AGE = (VISITDT - BRTHDTC)/365.25", 
                  "BMI = WEIGHT/(HEIGHT/100)^2", 
                  "LBORRESU = CONVERT_UNITS(LBORRES, LBORRESU, 'SI')"),
    Comment = c(NA, NA, NA)
  )
  
  # 関数をモックして、テストデータを返すようにする
  mock_read_excel <- function(spec, sheet) {
    return(test_data)
  }
  
  # 元の関数を保存
  original_read_excel <- readxl::read_excel
  
  # モック関数に置き換え
  assignInNamespace("read_excel", mock_read_excel, ns = "readxl")
  
  # 関数を実行
  result <- read_p21_spec_methods("dummy_file.xlsx", "Methods")
  
  # 元の関数を復元
  assignInNamespace("read_excel", original_read_excel, ns = "readxl")
  
  # 結果の検証
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  
  # 列名の検証
  expected_cols <- c("id", "name", "type", "description", "expression_context", "expression_code", "comment")
  
  # 実際の列名を取得
  actual_cols <- tolower(names(result))
  
  # 主要な列が存在することを確認
  essential_cols <- c("id", "name", "type", "description", "expression_context", "expression_code")
  for (col in essential_cols) {
    expect_true(any(grepl(col, actual_cols, fixed = TRUE)), 
                info = paste("Column", col, "not found in result"))
  }
  
  # データの検証
  expect_equal(result$id, c("METH001", "METH002", "METH003"))
  expect_equal(result$name, c("Calculate Age", "Derive BMI", "Convert Units"))
  expect_equal(result$type, c("Derivation", "Derivation", "Conversion"))
  
  # expression_context列の検証
  expression_context_patterns <- c("^expression_context$", "^context$")
  expression_context_col <- grep(paste(expression_context_patterns, collapse = "|"), actual_cols, value = TRUE)
  if (length(expression_context_col) > 0) {
    expect_equal(result[[expression_context_col[1]]], c("DM", "VS", "LB"))
  }
  
  # expression_code列の検証
  expression_code_patterns <- c("^expression_code$", "^code$", "^expression$")
  expression_code_col <- grep(paste(expression_code_patterns, collapse = "|"), actual_cols, value = TRUE)
  if (length(expression_code_col) > 0) {
    expect_equal(result[[expression_code_col[1]]], 
                c("AGE = (VISITDT - BRTHDTC)/365.25", 
                  "BMI = WEIGHT/(HEIGHT/100)^2", 
                  "LBORRESU = CONVERT_UNITS(LBORRES, LBORRESU, 'SI')"))
  }
})

test_that("read_p21_spec_comments returns correct data frame", {
  # テスト用のデータフレームを作成
  test_data <- data.frame(
    ID = c("COMM001", "COMM002", "COMM003"),
    Description = c("Protocol deviation", "Data quality issue", "Missing data"),
    Document = c("Protocol", "CRF", "Protocol"),
    Pages = c("Protocol-5", "CRF-10", "Protocol-8")
  )
  
  # 関数をモックして、テストデータを返すようにする
  mock_read_excel <- function(spec, sheet) {
    return(test_data)
  }
  
  # 元の関数を保存
  original_read_excel <- readxl::read_excel
  
  # モック関数に置き換え
  assignInNamespace("read_excel", mock_read_excel, ns = "readxl")
  
  # 関数を実行
  result <- read_p21_spec_comments("dummy_file.xlsx", "Comments")
  
  # 元の関数を復元
  assignInNamespace("read_excel", original_read_excel, ns = "readxl")
  
  # 結果の検証
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  
  # 列名の検証
  expected_cols <- c("id", "description", "document", "pages")
  
  # 実際の列名を取得
  actual_cols <- tolower(names(result))
  
  # 主要な列が存在することを確認
  essential_cols <- c("id", "description", "document", "pages")
  for (col in essential_cols) {
    expect_true(any(grepl(col, actual_cols, fixed = TRUE)), 
                info = paste("Column", col, "not found in result"))
  }
  
  # データの検証
  expect_equal(result$id, c("COMM001", "COMM002", "COMM003"))
  expect_equal(result$description, c("Protocol deviation", "Data quality issue", "Missing data"))
  expect_equal(result$document, c("Protocol", "CRF", "Protocol"))
  expect_equal(result$pages, c("Protocol-5", "CRF-10", "Protocol-8"))
})

test_that("read_p21_spec_documents returns correct data frame", {
  # テスト用のデータフレームを作成
  test_data <- data.frame(
    ID = c("DOC001", "DOC002", "DOC003"),
    Title = c("Study Protocol", "Statistical Analysis Plan", "Case Report Form"),
    Href = c("protocol.pdf", "sap.pdf", "crf.pdf")
  )
  
  # 関数をモックして、テストデータを返すようにする
  mock_read_excel <- function(spec, sheet) {
    return(test_data)
  }
  
  # 元の関数を保存
  original_read_excel <- readxl::read_excel
  
  # モック関数に置き換え
  assignInNamespace("read_excel", mock_read_excel, ns = "readxl")
  
  # 関数を実行
  result <- read_p21_spec_documents("dummy_file.xlsx", "Documents")
  
  # 元の関数を復元
  assignInNamespace("read_excel", original_read_excel, ns = "readxl")
  
  # 結果の検証
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  
  # 列名の検証
  expected_cols <- c("id", "title", "href")
  
  # 実際の列名を取得
  actual_cols <- tolower(names(result))
  
  # 主要な列が存在することを確認
  for (col in expected_cols) {
    expect_true(any(grepl(col, actual_cols, fixed = TRUE)), 
                info = paste("Column", col, "not found in result"))
  }
  
  # データの検証
  expect_equal(result$id, c("DOC001", "DOC002", "DOC003"))
  expect_equal(result$title, c("Study Protocol", "Statistical Analysis Plan", "Case Report Form"))
  expect_equal(result$href, c("protocol.pdf", "sap.pdf", "crf.pdf"))
})

test_that("read_p21_spec handles missing sheets correctly", {
  # モックデータを作成
  mock_sheets <- c("datasets", "variables", "codelists", "valuelevels", "dictionaries")  # methodsとcommentsが欠けている
  
  # モック関数を作成
  mock_excel_sheets <- function(path) {
    return(mock_sheets)
  }
  
  mock_file_exists <- function(path) {
    return(TRUE)
  }
  
  # 元の関数を保存
  original_excel_sheets <- readxl::excel_sheets
  
  # モック関数に置き換え
  assignInNamespace("excel_sheets", mock_excel_sheets, ns = "readxl")
  testthat::with_mocked_bindings(
    {
      # エラーメッセージをテスト
      expect_error(
        read_p21_spec("dummy.xlsx"),
        "This file does not appear to be a P21 specification file. Missing required sheets: Methods, Comments"
      )
    },
    file.exists = mock_file_exists,
    .package = "base"
  )
  
  # 元の関数を復元
  assignInNamespace("excel_sheets", original_excel_sheets, ns = "readxl")
})

test_that("read_p21_spec_define returns correct data frame", {
  # テスト用のデータフレームを作成
  test_data <- data.frame(
    Attribute = c("StudyName", "StudyDescription", "ProtocolName", "StandardName", "StandardVersion", "Language", "en", "", "Legend"),
    Value = c("CDISCPILOT01", "Study Data Tabulation Model Metadata Submission Guidelines Sample Study", 
              "CDISCPILOT01", "", "", "en", "", "", 
              "Highlighted cells are required for Define-XML 2.1 and can be ignored for prior versions.")
  )
  
  # 関数をモックして、テストデータを返すようにする
  mock_read_excel <- function(spec, sheet) {
    return(test_data)
  }
  
  # 元の関数を保存
  original_read_excel <- readxl::read_excel
  
  # モック関数に置き換え
  assignInNamespace("read_excel", mock_read_excel, ns = "readxl")
  
  # 関数を実行
  result <- read_p21_spec_define("dummy_file.xlsx", "Define")
  
  # 元の関数を復元
  assignInNamespace("read_excel", original_read_excel, ns = "readxl")
  
  # 結果の検証
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 6)  # 空白行以降は読み込まないため、6行になる
  
  # 列名の検証
  expected_cols <- c("attribute", "value")
  
  # 実際の列名を取得
  actual_cols <- tolower(names(result))
  
  # 主要な列が存在することを確認
  for (col in expected_cols) {
    expect_true(any(grepl(col, actual_cols, fixed = TRUE)), 
                info = paste("Column", col, "not found in result"))
  }
  
  # データの検証
  expect_equal(result$attribute, c("StudyName", "StudyDescription", "ProtocolName", "StandardName", "StandardVersion", "Language"))
  expect_equal(result$value, c("CDISCPILOT01", "Study Data Tabulation Model Metadata Submission Guidelines Sample Study", 
                              "CDISCPILOT01", "", "", "en"))
})

test_that("read_excel_all_sheets works correctly", {
  spec_file <- test_path("test_spec.xls")
  
  # テストデータの読み込み
  result <- read_excel_all_sheets(spec_file)
  
  # 基本的な構造のテスト
  expect_type(result, "list")
  expect_gt(length(result), 0)
  
  # 各シートがデータフレームであることを確認
  for (sheet_name in names(result)) {
    expect_true(inherits(result[[sheet_name]], "data.frame"),
               info = sprintf("Sheet '%s' is not a data frame", sheet_name))
  }
})

test_that("read_excel_all_sheets handles errors correctly", {
  # 存在しないファイルのテスト
  expect_error(read_excel_all_sheets("nonexistent.xls"), "The file does not exist")
  
  # 不正な拡張子のテスト
  temp_file <- tempfile(fileext = ".txt")
  file.create(temp_file)
  on.exit(unlink(temp_file))
  expect_error(read_excel_all_sheets(temp_file), "The file is not an Excel file")
}) 