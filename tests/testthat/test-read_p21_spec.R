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
