library(testthat)
library(dplyr)
library(tibble)

# サンプルデータとメタデータを準備
test_that("make_adam_dataset works as expected", {
  # 入力データ
  df <- tibble(
    USUBJID = c("01", "02", "03"),
    AGE = c(34, 45, 29),
    SEX = c("M", "F", "M"),
    HEIGHT = c(170, 160, 180)
  )

  # メタデータ
  adam_meta <- list(
    datasets = tibble(
      dataset = c("ADSL"),
      label = c("Subject-Level Analysis Dataset"),
      key = c("USUBJID")
    ),
    variables = tibble(
      dataset = c("ADSL", "ADSL", "ADSL"),
      variable = c("USUBJID", "AGE", "SEX"),
      label = c("Subject ID", "Age", "Sex"),
      type = c("text", "integer", "text"),
      order = c(1, 2, 3),
      format = c(NA, NA, NA),
      codelist = c(NA, NA, NA),
      origin = c(NA, NA, NA),
      length = c(10, 3, 1)
    )
  )

  # 関数を実行
  result <- make_adam_dataset(df, domain = "ADSL", adam_meta = adam_meta)
  print(result$USUBJID)

  # テスト: 必要な列が選択されているか
  expect_equal(colnames(result), c("USUBJID", "AGE", "SEX"))

  # # テスト: 並び替えが正しいか
  # expect_equal(result$USUBJID, c("01", "02", "03"))
  #
  # # テスト: ラベルが正しく設定されているか
  # expect_equal(attr(result$USUBJID, "label"), "Subject ID")
  # expect_equal(attr(result$AGE, "label"), "Age")
  # expect_equal(attr(result$SEX, "label"), "Sex")
  #
  # # テスト: データ型が正しいか
  # expect_type(result$USUBJID, "character")
  # expect_type(result$AGE, "double")
  # expect_type(result$SEX, "character")
})
