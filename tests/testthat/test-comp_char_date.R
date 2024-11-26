## Test 1: comp_char_date: compares two character dates ----
test_that("comp_char_date Test 1: compares two character dates", {
  expect_equal(comp_char_date("2024-01-01T10:01", "2024-01-01"), TRUE)
})

## Test 2: comp_char_dates: compares two character dates ----
test_that("comp_char_dates Test 2: compares two character dates", {
  expect_equal(comp_char_dates(c("2024-01-01T10:01", "2024-01-01"), c("2024-01-01T10:00", "2024-01-01")), c(FALSE, TRUE))
})
