datasets <- list()
datasets$df1 <- data.frame(
  KEY1 = c(1, 1, 1, 2, 2, 2, 3),
  KEY2 = c("A", "A", "B", "B", "C", "C", "D")
)
datasets$df2 <- data.frame(
  KEY1 = c(1, 1, 1, 2, 2, 2, 3),
  KEY2 = c("A", "A", "B", "B", "C", "C", "D"),
  KEY3 = c("a", "b", "b", "d", "d", "f", "f")
)

key_table <- data.frame(
  dataset = c("df1", "df2"),
  key = c("KEY1, KEY2", "KEY1, KEY2, KEY3")
)

result <- tibble(
  dataset = c("df1", "df2"),
  key = c("KEY1, KEY2", "KEY1, KEY2, KEY3"),
  dup = c(2, 0),
  description = c("1:2 5:6", "No duplicates found")
)

## Test 1: is_unique_key: checks if the key variables form a unique key in the data.frame ----
test_that("is_unique_key Test 1: checks if the key variables form a unique key in the data.frame", {

  expect_equal(is_unique_key(datasets$df1, split_key_text(key_table[[1, "key"]])), FALSE)
})

## Test 2: check_sdtm_keys: checks the key variables in SDTM datasets ----
test_that("check_sdtm_keys Test 2: checks the key variables in SDTM datasets", {

  expect_equal(check_sdtm_keys(datasets, key_table), result)
})
