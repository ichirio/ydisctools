library(dplyr)
## Test 1: check_invalid_chars: Check for Invalid Characters in a Data Frame(SDTM) ----
test_that("check_invalid_chars Test 1: Check for Invalid Characters in a Data Frame(SDTM)", {

  df <- data.frame(text = c("Hello\x01World", "Example?String"))
  result <- check_invalid_chars(df, target = text)
  expect_equal(result[[1, "inv_chars_ctrl"]], "[SOH]")
})
