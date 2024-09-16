library(dplyr)
## Test 1: conv_iso8601: Convert date and time(DMY) to ISO 8601 format ----
test_that("conv_iso8601 Test 1: Convert date and time(DMY) to ISO 8601 format", {
  df_test <- data.frame(
    SUBJID = sprintf("%03d", 1:13),
    testdate = c(
      "01-May-2024",
      "01/May/2024 23:01",
      "01/May/2024 23:1:1",
      "01/May/2024 23:1:1.000",
      "01 May 2024",
      "01.May.2024",
      "UN May 2024",
      "UN Unk 2024",
      NA,
      "",
      "2024",
      "24",
      "May 2024")
  )
  df_result <- data.frame(
    SUBJID = sprintf("%03d", 1:13),
    testdate = c(
      "01-May-2024",
      "01/May/2024 23:01",
      "01/May/2024 23:1:1",
      "01/May/2024 23:1:1.000",
      "01 May 2024",
      "01.May.2024",
      "UN May 2024",
      "UN Unk 2024",
      NA,
      "",
      "2024",
      "24",
      "May 2024"),
    TESTDTC = c(
      "2024-05-01",
      "2024-05-01T23:01",
      "2024-05-01T23:01:01",
      "2024-05-01T23:01:01.000",
      "2024-05-01",
      "2024-05-01",
      "2024-05",
      "2024",
      "",
      "",
      "2024",
      "2024",
      "2024-05")

  )

  expect_equal(mutate(df_test, TESTDTC = conv_iso8601(testdate)), df_result)
})

## Test 2: conv_iso8601: Convert date and time(YMD) to ISO 8601 format ----
test_that("conv_iso8601 Test 2: Convert date and time(YMD) to ISO 8601 format", {
  test_date = c(
    "2024/04/01",
    "2024/04/1 23:01",
    "2024/apr/2 23:1:1",
    "2024 4 1 23:1:1.000",
    "24/5/5",
    "99.5.5",
    "2024/4/UN",
    "2024/UN/un",
    NA,
    "",
    "2024",
    "24",
    "2024 May")
  test_result = c(
      "2024-04-01",
      "2024-04-01T23:01",
      "2024-04-02T23:01:01",
      "2024-04-01T23:01:01.000",
      "2024-05-05",
      "1999-05-05",
      "2024-04",
      "2024",
      "",
      "",
      "2024",
      "2024",
      "2024-05")

  expect_equal(conv_iso8601(test_date, type = "YMD"), test_result)
})

## Test 3: Convert Date to ISO 8601 Format (Date) ----
test_that("conv_iso8601_num_d Test 3: Convert Date to ISO 8601 Format (Date)", {

  expect_equal(conv_iso8601_num_d(as.Date("2024-01-01")), "2024-01-01")
})

## Test 4: Convert POSIXct to ISO 8601 Format (Date and Time without Seconds) ----
test_that("conv_iso8601_num_hm Test 4: Convert POSIXct to ISO 8601 Format (Date and Time without Seconds)", {

  expect_equal(conv_iso8601_num_hm(as.POSIXct("2024-1-1 1:1")), "2024-01-01T01:01")
})

## Test 5: Convert POSIXct to ISO 8601 Format (Date and Time with Seconds) ----
test_that("conv_iso8601_num_hms Test 5: Convert POSIXct to ISO 8601 Format (Date and Time with Seconds)", {

  expect_equal(conv_iso8601_num_hms(as.POSIXct("2024-1-1 1:1:1")), "2024-01-01T01:01:01")
})

